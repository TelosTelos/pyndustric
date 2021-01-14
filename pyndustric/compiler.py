import ast # standard python module for generating and using an abstract syntax tree (ast) representing python code

from dataclasses import dataclass, field # standard python module to ease storing attributes in classes
from collections import Counter   # dictionary subclass used to count instances of different types of label/variable
import inspect                    # standard python module used to retrieve source code from functions to compile them
import textwrap                   # standard python module used for changing indentation, e.g., of source code
from pathlib import Path           # standard python module for handling filenames to load compilable code from file
from typing import Callable, Union, List, Set # used for type-checking / debugging

from .constants import *          # definitions of various constants mapping Python entitites to mlog equivalents



class _Label(str):
    """This subclass of str stores information about a label that can be used to mark a destination for mlog jump
       commands. Each label itself is a string of the form "{labelname}", which is how it will appear in instructions
       until eventually being replaced with an actual line number.
       During its creation, each label creates a .destination no-op instruction, which can be included in a
       compilers ._main list of instructions as a placeholder to mark the destination for jumps involving this label.
       This class name is prefixed with an underscore to discourage accidental use of it.  New labels should
       should typically be created using compiler.Label(prefix) which will auto-generate a unique (within that
       compiler) name with that prefix, and call _Label() to create a label with that unique name."""

    name: str                    # this label's name, e.g., "endif_27"
    destination: "Instruction"   # the instruction that this label points to

    def __new__( cls, name ):
        lab = super().__new__(cls, '{'+name+'}') # the label is itself a string: its name wrapped in braces
        lab.name = name
        lab.destination = Instruction( lab )
        return lab


class Instruction(str):
    """This subclass of str stores information about an instruction line that is being prepared for inclusion
       in compiled mindustry assembly (masm) code.  Instructions will be listed in a compiler object's ._main
       attribute, and within various function objects, until eventually being concatenated to make output compiled code.
       An instruction created via Instruction( some_Label ) will be a no-op empty string, but will remember that
       Label as self.label .
       Each non-empty Instruction should be a valid mlog instruction *except* that line number destinations for jump
       instructions should instead have something of the form {labelname} in place of the line number, which
       will be replaced with an actual line number in the finalized output.  And *except* that instructions in
       function definitions will wrap argument names in braces, to enable substitution."""

    linenumber: int  #  Line number for this in the final output (or for next op instruction if this is no_op)
    label: _Label    #  Indicates what label if any, marks this instruction as its destination

    def __new__(cls, content ):
        if isinstance(content, Instruction): return content
        if isinstance( content, _Label ):
            i = super().__new__(cls, "")
            i.label = content
        else:
            i = super().__new__(cls, content)
            i.label = None
        return i

    def __repr__(self): return self if self else ( f"LABEL: {self.label.name}" if self.label else "" )

    @property
    def is_non_continuing(self):
        """Returns true if this instruction never lets flow of execution flow on to the next instruction, e.g.
           because it jumps away always, because it sets the @counter to a new location, or because it end's"""
        if self == "end": return True
        if self.startswith("jump") and self.endswith("always"): return True
        if self.startswith("set @counter"): return True
        if self.startswith("op @counter"): return True
        return False


class PartialMap( dict ):
    """This is much like an ordinary dict except for any failed attempt to look up a string, this returns
       that string rewrapped in {}.  This sort of mapping can be used with Python's str.format to substitute
       values for specified {}-wrapped items, while leaving unspecified {}-wrapped items unchanged."""
    def __missing__(self, key):
        return '{' + key + '}'


class Function:
    """This stores information about a user-defined function that is callable within a compilable program."""

    def __init__(self, name:str, mode:str, args:List[str] = None):
        self.name = name # name of this function, e.g. 'foo'
        self.mode = mode # compilation mode, 'inline' or 'function'
        self.call_label = _Label(f"__{name}_CALL")
        self.end_label = _Label(f"__{name}_END")
        self.return_label = _Label(f"__RETURN_FROM_{name}") # won't be used as dest, but helps optimization
        self.return_value = f"__{name}_RETURN_VALUE"
        self.return_line = f"__{name}_RETURN_LINE"
        self.args: List[str] = args if args else [] # list of args the function takes
        self.ins: List[Instruction] = [] # list of instructions in body of function, with args in {}
        self.labels: Set[_Label] = set() # set of destination labels in body of function (must be mangled when inline)
        self.instances = 0 # count of how many times an inline function is put inline, used for mangling

    def jump_back_instruction(self, at_end = False):
        if self.mode == 'inline':
            if at_end: return None # an inline function at its end will automatically advance the right place
            return f'jump {self.end_label} always' # jump to the end of this inline function
        else: # jumpy functions jump back to whatever line the caller stored in the var named by self.return_line
            return f'set @counter {self.return_line}'

    @property
    def extended_args(self): return self.args+[self.return_value]

    def mangle(self, arg):
        """Returns a mangled version of arg, to ensure that arguments used in this function won't collide with
           like-named variables elsewhere in the program, for non-inline functions."""
        return f"__{self.name}_{arg}"

    def mapped_ins(self, mapping={}):
        """Returns the list of self's instructions, with arguments and return_value mapped to their destinations in the
           given mapping.  Any args not given in mapping will be mapped to themselves."""
        if self.mode == "inline":
            m={a: a for a in self.args} # start with default map that takes each arg to itself
            # we must mangle labels for jumps within this inline instance to avoid cross-jumping with other instances
            labelmap = {L.name : _Label('__'+self.name+'_'+str(self.instances)+'_'+L.name) for L in self.labels }
            m.update( labelmap ) # now we'll mangle the labels in jump commands (but not their destinations yet)
            self.instances += 1  # next instance of this inline fn, the mangled labels will be one higher
        else:
            m={a: self.mangle(a) for a in self.args} # mangle the name of each arg to differ from globals
            labelmap={} # no need to mangle labels for out-of-line functions
        m[self.return_value] = self.return_value # map the return_value to itself
        m.update( mapping )  # now override defaults with anything in the given mapping
        m = PartialMap(m) # m will now rewrap any unfound items, like jump labels, in {} so they can be subbed later

        # now we can make all the substitutions described above, formatting op instructions, and labelmapping labels
        return list(map(Instruction, ((i.format_map(m) if i else
                                       labelmap.get(i.label.name if i.label else None,i)) for i in self.ins )))





class CompilerError(ValueError):
    def __init__(self, code, node: ast.AST):
        super().__init__(f'[{code}/{node.lineno}:{node.col_offset}] {ERROR_DESCRIPTIONS[code]}')


class Compiler(ast.NodeVisitor):
    """ast.NodeVisitor is a superclass with methods that enable straightforward visitation of the various nodes
    in an AST syntax tree, calling corresponding methods for the different types of nodes as they're visited, where
    parent nodes typically opt to initiate visits for their children.  This subclass overwrites the default visit
    methods with ones that generate compiled mlog code as the various parts of the tree are visited."""
    def __init__(self):
        # self._main = [ Instruction( f'set {REG_STACK} 0') ] # initialize the stack # stack has been removed
        self._main:List[Instruction] = [] # list of instructions that will compose main body of compiled code
        self._current = self._main        # list of instructions we're currently adding to: main / function body
        #TODO I'm not sure we really want/need a stack by default -- probly better to do everything with dummy variables
        self._in_def = None   # will be set to a True-ish value while visiting user-defined function definitions
        self._functions = {}  # will map user defined function names to Function objects containing info about functions
        self._prefix_counter = Counter() # will track uses of different prefixes for unique Labels and dummy variables

    def Label(self, prefix):
        """This returns a newly constructed label with a unique (to this compiler) name, beginning with prefix.
           Labels are used to mark destinations for jump commands.  Once line numbering is finalized, labels in
           jump commands will be replaced with actual line numbers."""
        index = self._prefix_counter[prefix]
        self._prefix_counter[prefix] += 1  # increment the count of uses of this prefix, so the next will differ
        return _Label(f"{prefix}_{index}")

    def Dummy(self, prefix):
        """This returns a newly constructed dummy variable name, unique (in this compiler), beginning with prefix.
        """
        index = self._prefix_counter[prefix]
        self._prefix_counter[prefix] += 1  # increment the count of uses of this prefix, so the next will differ
        return f"__{prefix}_{index}"

    def ins_append( self, content ):
        """This appends an Instruction (or list thereof) with content content to whatever ._current list of
           instructions we are currently adding to, either the main program or a function body."""
        if content is None: return # no point in appending completely content-less instructions
        if not isinstance(content, list): content=[content] # wrap single instruction in singleton list
        for i in content:
            if not isinstance( i, Instruction ): i = Instruction(i) # coerce i to instruction
            self._current.append( i )
            if self._in_def:
                if i.label: self._in_def.labels.add(i.label) # remember this label is jump destination within function


    def compile(self, code: Union[str, Callable, Path]):
        """This is the main function for the compiler taking in a chunk of Python code (specified either as a
        string, as a function whose source code file can be found via python's inspect module,
        or TODO as a Path indicating a source code file to compile.
        This visits various nodes in the syntax tree of that code, amassing a list of mlog instructions that will
        perform the equivalent of that code in mindustry.
        This list is then concatenated into a single string, and returned."""

        # TODO enable this to handle function and Path inputs, not just strings
        body: list[ast.AST]  # will store one or more AST nodes composing the body of code to be compiled


        if callable(code): # if code is given as a function, we'll compile its body (ignoring def and docstring)
            code = textwrap.dedent( inspect.getsource(code) ) # looks up function's source code from file
            body = ast.parse(code).body[0].body # i.e., tree.body_of_tree[0th stmt, namely "def..."].body_of_function
            if ( isinstance( body[0], ast.Expr ) and
                 isinstance( body[0].value, ast.Constant ) and
                 isinstance( body[0].value.value, str) ):
                body = body[1:] # if zeroth statement in function body is a docstring, bypass it
        else: # if we were given a string to compile:
            body = ast.parse(code).body # list of statements (ast nodes) composing the body of the given string

        for node in body:  # visit successive statements in the body of code we're compiling
            self.visit(node)

        # TODO insert any optimization pass(es) we want to make before finalizing line numbers

        masm = self.generate_masm()  # concatenate our amassed list of instructions into a single string to return

        print("\n-----Compiled code: -----------------")
        print(masm)
        print("-------------------------------------")
        return masm

        # TODO print the resulting mlog instructions to console, so user can read or copy/paste
        # TODO place a copy of the resulting mlog string in the clipboard, so it can be pasted to Mindustry

    def visit(self, node):
        """This will call some specific flavor of visit method, depending upon what type of node this is, typically
        one of the ones defined below."""
        super().visit(node)

    def visit_Import(self, node):
        """This will be called for any* import statement in the given code (*aside from ones buried in parts of the
        syntax tree that end up not being visited, typically due to other errors -- a similar qualifier would apply
        to other specific visit methods, but will not be spelled out again)."""
        # TODO remember what files are imported, and that we may need to look there for source code for helper functions
        #      so that we can compile that into the mlog program too
        # TODO at worst this should probably be a warning rather than an error
        raise CompilerError(ERR_UNSUPPORTED_IMPORT, node)

    def visit_ImportFrom(self, node):
        """This will be called for any* statement of the form "from {module} import {names}.
           Currently, this allows only for importing everything from the pyndustri.pyi interface module."""
        # TODO again if we want to allow code to import other functions and end up compiling them, we'll probably
        #      need to allow these imports and track where to go to find the relevant function code to compile
        if node.module != 'pyndustri' or len(node.names) != 1 or node.names[0].name != '*':
            raise CompilerError(ERR_UNSUPPORTED_IMPORT, node)

    def visit_Assign(self, node: ast.Assign):
        """This will be called for any* statement of the form "{targets} = {values}"
        """
        target = self.as_target( node.targets[0] ) # the variable we'll assign a value to, e.g., the x in "x = 1"

        self.create_Assign( target, node.value ) # Create instruction(s) to set target = value

        if len(node.targets) > 1: # if this was a multi-assignment like x = y = 0
            # TODO ERR_MULTI_ASSIGN was raised here, but now this compiles fine. Could delete this error + its test.
            for additional_target in node.targets[1:]:
                t = self.as_target(additional_target)
                self.ins_append(f"set {t} {target}") # set the later targets to mimic first

    def create_Assign(self, variable: str, newvalue: ast.AST):
        """Appends one or more Instructions to assign to variable the value specified in AST node newvalue.
           This is used to compile explicit assignment to a user-specified variable, and also to assign values
           to dummy variables to store intermediate results in complex expressions, and
           TODO in passing arguments and return values to/from functions.
           The division of labor is this: (1) something else, e.g. visit_Assign, determines that we should have
           instructions assign some value to a variable, (2) create_Assign figures out which mlog instruction would
           be best suited for doing this, usually set, op, or sensor, and it calls (3) as_value to figure out what
           value(s) will need to be plugged into the remaining gap(s) in that instruction, often inserting preceding
           instructions to store intermediate values for complex expressions in dummy variables, another instance of (1).
        """

        # print(f"\nCreating assignment instruction(s) for {variable} = {ast.dump(newvalue)}")

        if isinstance(newvalue, ast.BinOp):  # if expression is a binary operation, e.g., a+b
            op = BIN_OPS.get(type(newvalue.op)) # look up equivalent op in dict of mlog binary ops, or None if not there
            if op is None:
                raise CompilerError(ERR_UNSUPPORTED_OP, newvalue)

            left = self.as_value(newvalue.left)
            right = self.as_value(newvalue.right)
            self.ins_append(f'op {op} {variable} {left} {right}') # e.g. "x=y+z" compiles to "op add x y z"

        elif isinstance(newvalue, ast.Compare):  # if expression is a binary comparison, e.g., a<b
            if len(newvalue.ops) != 1 or len(newvalue.comparators) != 1:
                raise CompilerError(ERR_UNSUPPORTED_EXPR)
            #TODO not sure what exactly is being ruled out here? things like a<b<c?  Could be good to support those.

            cmp = BIN_CMP.get(type(newvalue.ops[0])) # look up the mlog equivalent of this python comparison, or None
            if cmp is None:
                raise CompilerError(ERR_UNSUPPORTED_OP, newvalue)

            left = self.as_value(newvalue.left)
            right = self.as_value(newvalue.comparators[0])
            self.ins_append(f'op {cmp} {variable} {left} {right}') # e.g. "x = y<z" compiles to "op lessThan x y z"

        elif isinstance(newvalue, ast.Call):  # if newvalue is a function/method call, e.g. abs(x) or Sensor.copper(...)
            if isinstance(newvalue.func, ast.Attribute): # if newvalue has the form obj.method(args)
                obj = newvalue.func.value.id
                method = newvalue.func.attr
                if obj == 'Unit':    # Unit.method( args )
                    if method == "radar":
                        self.radar_instruction(variable, obj, newvalue)
                    elif method in UNIT_FIND_METHODS:
                        pass #TODO
                    # TODO are there any other Unit.methods that return a value?
                elif obj == 'Sensor':  # Sensor.method( args )
                    if len(newvalue.args) != 1:
                        raise CompilerError(ERR_ARGC_MISMATCH, newvalue)

                    arg = newvalue.args[0].id

                    attr = RES_MAP.get( method ) # e.g. if we're doing Sensor.copper(block), this will be '@copper'
                    if attr is None:
                        raise CompilerError(ERR_UNSUPPORTED_SYSCALL, newvalue)

                    self.ins_append(f'sensor {variable} {arg} {attr}')
                    # TODO now that there is a cleaner object.attribute interface for this, may want to deprecate Sensor?
                else:  # if the object whose method is being called isn't one with idiomatic methods
                    if method == "radar":
                        self.radar_instruction(variable, obj, newvalue)

                    # TODO are there any other obj.methods that return values?
                    else:  # couldn't figure out any way to handle this obj.method call
                        raise CompilerError(ERR_NO_DEF, newvalue )

            elif isinstance(newvalue.func, ast.Name):  # if newvalue has the form functionname(args)
                function = newvalue.func.id
                if function in FUNCTION_OPS: # e.g. abs(x) or dst(x,y)
                    self.ins_append(f"op {function} {variable} " +
                                    ' '.join(self.as_value(arg) for arg in newvalue.args))

                elif function in self._functions:  # if this is in our repository of user-defined functions
                    fn = self._functions[function]

                    if len(newvalue.args) != len(fn.args):
                        raise CompilerError(ERR_ARGC_MISMATCH, newvalue)

                    if fn.mode == "inline":
                        # For inline macrolike functions, we map each instance of an arg in the function's body to the
                        # given value for that arg, and map the return value to the variable we're aiming to store
                        # newvalue into, and then we'll plunk the resulting code inline right here.
                        # Note: variable values will appear as-is in the function, so may be altered by it.
                        # Complex values will be set to temp variables, so are safe from this.  Constant values will
                        # be substituted into the place of their args within the function body.
                        # TODO could be good to raise an error when passing constant value to arg that fn reassigns
                        mapping = { arg: self.as_value(node) for arg, node in zip( fn.args, newvalue.args ) }
                        mapping[fn.return_value] = variable # wherever fn sets its return, it'll set variable for us
                        self.ins_append( fn.mapped_ins( mapping ) )

                    else: # if this is a jumpy out-of-line function
                        for arg,value in zip(fn.args, newvalue.args): # assign/"pass" values for each (mangled) arg
                            self.create_Assign( fn.mangle(arg), value )
                        self.ins_append(f'op add {fn.return_line} @counter 1') # Tell fn where it should jump back to
                        self.ins_append(f'jump {fn.call_label} always')        # Jump to the start of fn's body
                        # After executing, and setting its return_value, the function will jump back here
                        self.ins_append(fn.return_label) # This label won't be a destination, but helps optimization
                        self.ins_append(f'set {variable} {fn.return_value}')   # set variable = return_value: Job done!

                else:  # couldn't figure out any way to handle this function call
                    raise CompilerError(ERR_NO_DEF, newvalue)

        # TODO determine where to handle any other object-attribute based calls, like Unit.approach() or salvo1.shoot()?
        # TODO allow calling of user-defined functions at beginning of expression, not just in assignment

        elif isinstance(newvalue, ast.Attribute): # e.g., vault1.copper
            # The ast should be structured like this: newvalue = Attribute(value=Name(id='vault1'), attr='copper' )
            # TODO may want to generate our own compiler errors for type mismatches?
            obj:str =  newvalue.value.id # e.g. vault1
            attr:str = newvalue.attr     # e.g. copper

            # TODO vectorize operations on vector attributes .pos .shootPos and .minePos
            # TODO screen other idiomatic object.attribute references that shouldn't compile to sensor instructions?

            mlog_attr = '@'+attr.replace('_','-') # e.g. "phase_fabric" --> "@phase-fabric"
            self.ins_append(f'sensor {variable} {obj} {mlog_attr}') # "x = vault1.copper"->"sensor x vault1 @copper"

        else:  # for any other value
            val = self.as_value(newvalue, allow_dummy=False)  # "set x new_dummy" would be useless and circular
            self.ins_append(f'set {variable} {val}')  # and compile this to "set variable val"

    def radar_instruction(self, variable, obj, newvalue):
        """This will be called for any instance where a variable will be set to a radar reading from object obj,
           which will either be 'Unit' or the name of some Block, which will compile to uradar or radar, respectively.
           Newvalue will be the ast node for this radar call.  This will extract the relevant arguments from that node.
           This treats the method as though it had the following def radar( self, criterion1, criterion2=any,
           criterion3=any, order=1, key=distance ), so later criteria, order and key are optional.  The order
           boolean accepts min and max as values.
        """
        # The newvalue ast node should have the following structure:
        # Call(func=Attribute(value=Name(id='Unit'), attr='radar'),
        #      args=[ Name(id='enemy'), Name(id='flying') ],
        #      keywords=[keyword(arg='order', value=Name(id='min')),
        #                keyword(arg='key', value=Name(id='distance'))])

        if obj=="Unit":
            radar = "uradar" # radar from bound Unit uses "uradar" instruction
            obj = "@unit" # the text version of uradar apparently still demands an actual object anyway
        else:
            radar = "radar" # radar from blocks uses radar instruction, and the block as the object

        criteria = ' '.join( arg.id for arg in newvalue.args )
        while criteria.count(' ')<2: criteria += " any"  # mlog requires 3 criteria, even if they're "any"

        kwargs = dict(order='min', key='distance') # default values for keyword args
        kwargs.update({ k.arg : self.as_value(k.value) for k in newvalue.keywords}) # overwrite with given keyword args
        # print(f"\nUsing keyword args: {kwargs}" )
        kwargs['order'] = RADAR_ORDERS.get( kwargs['order'] ) # order can accept unusual values like min, max
        if kwargs['order'] is None:
            raise CompilerError(ERR_UNSUPPORTED_EXPR, newvalue )

        if kwargs['order'] in ['min','max']:  # we allow min and max as order args, map to 1,0
            kwargs['order'] = 1 if kwargs['order'] == 'min' else 0
        self.ins_append(f"{radar} {criteria} {kwargs['key']} {obj} {kwargs['order']} {variable}")

    def visit_AugAssign(self, node: ast.Assign):
        """This will be called for any* augmenting assignment statement, like "x += 1"  """
        target = self.as_target( node.target ) # e.g., x in "x += 1"
        # if not isinstance(target, ast.Name):
        #     raise CompilerError(ERR_COMPLEX_ASSIGN, node)

        op = BIN_OPS.get(type(node.op))
        if op is None:
            raise CompilerError(ERR_UNSUPPORTED_OP, node)

        right = self.as_value(node.value)
        self.ins_append(f'op {op} {target} {target} {right}')
        # TODO this needs to allow for complex expressions on the right-hand side

    def conditional_jump(self, destination_label, test, jump_if_test = True):
        """This adds a conditional jump instruction to destination_label based on the ast node test.
           When jump_if_test == True, we jump if the test is passed; when False we instead jump if test fails.
           When a jump is not triggered, mlog flow of execution will instead advance to whatever comes next.
           This is used for compiling various conditional jumps, e.g. in 'if' and 'while' commands."""
        if isinstance(test, ast.Compare):  #mlog lets us incorporate a comparison into a jump statement
            if len(test.ops) != 1 or len(test.comparators) != 1:
                raise CompilerError(ERR_UNSUPPORTED_EXPR)
            cmp = BIN_CMP.get(type(test.ops[0]))
            left = self.as_value(test.left)
            right = self.as_value(test.comparators[0])
        elif isinstance(test, ast.BoolOp):  # we'll manually compile greedy evaluation of 'and' and 'or'
            cmp = BIN_CMP.get(type(test.op))
            left, right = self.as_value(test.values[0]), self.as_value(test.values[1])
        else: # our test is already (something interpretable as) boolean, so any nonzero test counts as "True"
            left, cmp, right = self.as_value(test), "notEqual", 0

        if jump_if_test==False: cmp = NEGATED_BIN_CMP.get( cmp )
        if cmp is None:
            raise CompilerError(ERR_UNSUPPORTED_OP, test)

        if cmp=='and': #we could use mindustry's land, but python and-evaluation is supposed to be lazy
            #TODO if right arg is simple enough, no point in lazy evaluation, slightly shorter/faster to use mlog's land
            failed_label = self.Label("one_is_false")
            self.ins_append(f"jump {failed_label} equal {left} 0 ")  # if left conjunct false, test failed so no jump
            self.ins_append(f"jump {destination_label} notEqual {right} 0 ") # both conjuncts were true, so jump!
            self.ins_append( failed_label ) # if test failed we continue without jumping to destination
        elif cmp=='or':
            self.ins_append(f"jump {destination_label} notEqual {left} 0 ")  # one true disjunct is enough, let's jump!
            self.ins_append(f"jump {destination_label} notEqual {right} 0 ") # or the other works too!
        elif cmp=='nand': # we're supposed to jump if either arg is false
            self.ins_append(f"jump {destination_label} equal {left} 0 ")  # one being false is enough, let's jump!
            self.ins_append(f"jump {destination_label} equal {right} 0 ") # or the other works too!
        elif cmp=='nor': # we're supposed to jump if neither arg is true, i.e. if both are false
            failed_label = self.Label("one_is_true")
            self.ins_append(f"jump {failed_label} notEqual {left} 0 ")  # if left conjunct true, test failed, so no jump
            self.ins_append(f"jump {destination_label} Equal {right} 0 ") # both conjuncts were true, so jump!
            self.ins_append( failed_label )
        else: # we can just use the built-in mlog binary comparison cmp
            self.ins_append(f'jump {destination_label} {cmp} {left} {right}')


    def visit_If(self, node):
        """This will be called for any* if statement, like "if test: body" potentially with elif/else clauses."""
        endif_label = self.Label("endif") # generate a new unique label to mark the end of this if statement
        if_false_label = self.Label("else") if node.orelse else endif_label # label to jump to if test is false
        self.conditional_jump( if_false_label, node.test, jump_if_test = False ) # if test fails, skip past body
        for subnode in node.body:  # compile the "body" (commands to execute if test was true)
            self.visit(subnode)
        if node.orelse: # if there is an else (or elif) clause
            if not self._current[-1].is_non_continuing: # if the if-true condition didn't jump away on its own
                self.ins_append(f'jump {endif_label} always') # jump from body down past else clause when test was true
            self.ins_append( if_false_label )             # mark where we should jump to when test is false
            for subnode in node.orelse:  # compile the else-clause (commands to execute if test was false)
                self.visit(subnode)
        self.ins_append(endif_label) # mark where to jump to if we need to skip past whatever preceded this

    def visit_While(self, node):
        """This will be called for any* while loop."""
        body_label = self.Label("while_body") # will mark the beginning of the body of the loop
        end_label = self.Label("while_end")   # will mark where to go after escaping loop
        self.conditional_jump(end_label, node.test, jump_if_test=False) # if test starts false skip loop entirely
        self.ins_append(body_label) # mark beginning of body of while loop
        for subnode in node.body:
            self.visit(subnode)
        self.conditional_jump(body_label, node.test, jump_if_test=True) # if test is still true, loop back up
        self.ins_append(end_label) # mark end of while loop

        # TODO: In a case where the test is complex, we could trade away a tiny bit of speed to get a large reduction
        #       in instruction count by not duplicating the test.  However, instruction count matters only when a
        #       a program exceeds the maximum allowable count, and this will rarely be the straw that breaks a
        #       camel's back, so we're probably fine sticking with a preference for speed?

    def visit_For(self, node):
        """This will be called for any* for loop, though not many will be supported because mlog doesn't offer much
           in the way of iterators or iterable lists.  The two iterables that are supported are range(start, stop, step)
           and Links (which iterates through various linked blocks)."""
        # TODO  It would be fairly natural to construe Unit.bind_next as an iterator, and to articulate code that
        #       cycles through units with the idiom "for Unit in UnitType( flare ):" so it might make sense to offer
        #       this as another pre-canned for-loop option.  Of course there are other ways of writing this, but
        #       the same is true of iterating over links

        target = self.as_target(node.target)
        # if not isinstance(target, ast.Name):
        #     raise CompilerError(ERR_COMPLEX_ASSIGN, node)
            # TODO: again the problem isn't that "for 12 in range(5):" is *complex*, it's that 12 isn't the right sort of thing to assign values to

        call = node.iter
        if not isinstance(call, ast.Call):
            raise CompilerError(ERR_UNSUPPORTED_ITER, node)

        body_label = self.Label("for_body") # will mark the beginning of the body of the loop
        end_label = self.Label("for_end")   # will mark where to go after escaping loop

        inject = []

        if isinstance(call.func, ast.Attribute) \
                and call.func.value.id == 'Env' and call.func.attr == 'links':
            it = REG_IT_FMT.format(call.lineno, call.col_offset)
            start, end, step = 0, '@links', 1
            inject.append(f'getlink {target} {it}')
        elif isinstance(call.func, ast.Name) and call.func.id == 'range':
            it = target
            argv = call.args
            argc = len(argv)
            if argc == 1:
                start, end, step = 0, self.as_value(argv[0]), 1
            elif argc == 2:
                start, end, step = *map(self.as_value, argv), 1
            elif argc == 3:
                start, end, step = map(self.as_value, argv)
            else:
                raise CompilerError(ERR_BAD_ITER_ARGS, node)
        else:
            raise CompilerError(ERR_UNSUPPORTED_ITER, node)

        self.ins_append(f'set {it} {start}') # set iterating variable to initial value
        self.ins_append(f'jump {end_label} greaterThanEq {it} {end}') # if already past end skip loop entirely
        # TODO  if the step in range(start,end,step) is negative, the relevant test is lessThanEq
        self.ins_append(body_label) # mark beginning of body of for loop
        self.ins_append(inject)     # append beginning of loop instructions, prepared above, if any
        for subnode in node.body:   # compile body of loop
            self.visit(subnode)
        self.ins_append(f"op add {it} {it} {step}")            # increment the iterator
        self.ins_append(f"jump {body_label} lessThan {it} {end}")   # if still before end, loop back up
        # TODO  if the step in range(start,end,step) is negative, the relevant test is greaterThan
        self.ins_append(end_label)  # mark end of for loop, in case first test skipped to here

    def visit_FunctionDef(self, node):
        """This will be called for any* function definition of the form def fname( args ): body.
           Pyndustric offers two compile modes for def statements, selectable by preceding the def-line with
           one of the following decorators @inline, or @function.
           The default mode @inline generally produces the fastest-executing mlog code.  This macro-like mode replaces
           each function call with the body of the function, substituting the given arguments into that body wherever
           relevant.  This has the consequence that, when a variable is passed as an argument and that argument is
           assigned a new value in the body of the function, that variable will have that new value even after the
           function returns, which differs from standard Python functions.  If you want to mask a variable so that it
           can't be altered by an inline macro, precede it with 0+ in the function call.  E.g., inc(0+y) will first
           set some temporary variable, temp = 0+y, and then process inc(temp), which may affect temp, but will not
           affect y itself (unless the function specifically alters the global y).
           The second @function mode instead compiles the function as a separate block of code, and uses various
           set and jump instructions to pass argument values to the function and to jump to and from the function.also replaces each function call with the body of
           the function, but replaces variable arguments with temp variables, so that assigning new values to args
           in the function will not changethat function, that variable
           will end up with a new l"""

        if self._in_def: # if we're already in the midst of defining another function
            raise CompilerError(ERR_NESTED_DEF, node)
        # TODO there's no reason we *couldn't* allow nested function defns, though it may be more tricky than it's worth

        name = node.name # the name of this function
        if name == 'print' or name in FUNCTION_OPS or name in self._functions:
            raise CompilerError(ERR_REDEF, node)

        # Currently this will end up making mode=="inline" (the default) or else mode=="function"
        mode = DEF_COMPILE_MODES["default"]
        if ( node.decorator_list and isinstance( node.decorator_list[0], ast.Name)):
            mode = DEF_COMPILE_MODES.get( node.decorator_list[0].id )
            if not mode: raise CompilerError(ERR_UNSUPPORTED_EXPR, node.decorator_list[0])
            # TODO create more informative unknown decorator error

        # mode actually doesn't play much role in function *definition*, since that just compiles the def body into
        # the Function object (including remembering mode).  mode plays a big role in how function *calls* compile!

        # return value variable is now specified as fn.return_value, created in Function.__init__
        # reg_ret = f'{REG_RET_COUNTER_PREFIX}{len(self._functions)}'

        args = node.args
        if any((
                args.posonlyargs,
                args.vararg,
                args.kwonlyargs,
                args.kw_defaults,
                args.kwarg,
                args.defaults,
        )):
            raise CompilerError(ERR_INVALID_DEF, node)
        # TODO  many of the above could/should be supported

        # TODO it's better to put functions at the end and not have to skip them as code, but jumps need fixing
        # self.ins_append('jump {} always')

        # this is now handled with fn.call_label
        # prologue = len(self._main)  # line number where the function definition begins

        # this is no longer used, is equiv to len(fn.args)
        # argc=len(args.args)

        fn = self._functions[name] = Function(name = name, mode=mode, args=[a.arg for a in args.args] )
        self._in_def = fn # remember that we're now in the midst of defining this function
        self._current = fn.ins # for now ins_append will add to this function's body, rather than to self._main

        if mode == "function":
            self.ins_append(fn.call_label) # jumpy functions need a label for calls to jump to

        # # return line is now stored by caller in the variable named in fn.return_line
        # self.ins_append(f'read {reg_ret} cell1 {REG_STACK}') #TODO to allow recursion this would need to stay on stack

        # # arguments should now be set by the caller, so no need to unpack from stack!
        # for arg in args.args:
        #     self.ins_append(f'op sub {REG_STACK} {REG_STACK} 1')
        #     self.ins_append(f'read {arg.arg} cell1 {REG_STACK}')

        for subnode in node.body:
            self.visit(subnode) # when these call ins_append, instructions will be appended to this function's body

        # there is no longer a separate epilogue to jump to; instead returns jump straight back to sender
        # epilogue = len(self._main)
        # for i in range(prologue, epilogue):
        #     if '{epilogue}' in self._main[i]:
        #         self._main[i] = self._main[i].format(epilogue=epilogue)

        if fn.ins and fn.ins[-1].is_non_continuing: # if fn already jumped away...
            if mode=="inline" and fn.ins[-1]==fn.jump_back_instruction(at_end=False):
                fn.ins.pop() # when inline fn ends with return, can eliminate pointless jump to its own end!
        else: # if the function didn't jump away on its own we'll need to return for it
            # TODO Python functions return None when reach end of body.  Is 0 the best mlog equivalent?
            self.ins_append( f'set {{{fn.return_value}}} 0' ) # wrapped in braces for substitution
            self.ins_append( fn.jump_back_instruction(at_end = True) )

        if mode == "inline":
            self.ins_append( fn.end_label ) # any return commands in inline function will have tried to jump here

        # jumpy functions are now appended at end of program so we didn't have to skip past function body at beginning
        # end = len(self._main)
        # self._main[prologue - 1] = self._main[prologue - 1].format(end)

        self._in_def = None # back to reading statements as part of main program
        self._current = self._main # now ins_append will add to main program, rather than this function's body

    def visit_Return(self, node):
        """This will be called for any instance of Return, typically from within a function definition."""

        if not self._in_def: raise CompilerError(ERR_NO_DEF, node)
        # TODO is this the right error to raise for return when not in the midst of a def?
        #      note: if source code is given as a def, top-level return will look perfectly syntactic to Python

        self.create_Assign( '{'+self._in_def.return_value+'}', node.value ) # wrapped in braces for substitution
        self.ins_append( self._in_def.jump_back_instruction(at_end = False) )
        # self.ins_append('jump {epilogue} always')


    def visit_Expr(self, node):
        """This will be called for any* instance of an Expr, *** whatever exactly that is???."""
        # TODO  figure out what all counts as an Expr, and figure out how to combine this with a general ability
        #       to process complex expressions
        call = node.value
        if not isinstance(call, ast.Call):
            raise CompilerError(ERR_UNSUPPORTED_EXPR, node)

        # `print`, unlike the rest of syscalls, has no namespace
        if isinstance(call.func, ast.Name):
            if call.func.id == 'print':
                return self.emit_print_syscall(call)
            else:
                return self.as_value(call)

        if not isinstance(call.func, ast.Attribute) or not isinstance(call.func.value, ast.Name):
            raise CompilerError(ERR_UNSUPPORTED_EXPR, node)

        ns = call.func.value.id
        if ns == 'Screen':
            self.emit_screen_syscall(call)
        elif ns == 'Control':
            self.emit_control_syscall(call)
        else:
            raise CompilerError(ERR_UNSUPPORTED_SYSCALL, node)

        # TODO  This may be the place to handle other (pseudo-)function calls, like dst(), abs(),
        #       end(), label(), and jump_to()

    def emit_print_syscall(self, node: ast.Call):
        """This will be called by visit_Expr when it encounters a print(...) command."""
        if len(node.args) != 1:
            raise CompilerError(ERR_BAD_SYSCALL_ARGS)

        arg = node.args[0]
        if isinstance(arg, ast.JoinedStr):
            for value in arg.values:
                if isinstance(value, ast.FormattedValue):
                    if value.format_spec is not None:
                        raise CompilerError(ERR_BAD_SYSCALL_ARGS, node)

                    val = self.as_value(value.value)
                    self.ins_append(f'print {val}')
                elif isinstance(value, ast.Constant):
                    val = self.as_value(value)
                    self.ins_append(f'print {val}')
                else:
                    raise CompilerError(ERR_BAD_SYSCALL_ARGS, node)
        else:
            val = self.as_value(arg)
            self.ins_append(f'print {val}')

        flush = True
        for kw in node.keywords:
            if kw.arg == 'flush':
                if isinstance(kw.value, ast.Constant) and kw.value.value in (False, True):
                    flush = kw.value.value
                elif isinstance(kw.value, ast.Name):
                    flush = kw.value.id
                else:
                    raise CompilerError(ERR_BAD_SYSCALL_ARGS, node)
            else:
                raise CompilerError(ERR_BAD_SYSCALL_ARGS, node)

        if isinstance(flush, str):
            self.ins_append(f'printflush {flush}')
        elif flush:
            self.ins_append(f'printflush message1')

    def emit_screen_syscall(self, node: ast.Call):
        """This will be called by visit_Expr when it encounters a Screen.attr(...) command."""
        method = node.func.attr
        if method == 'clear':
            if len(node.args) != 3:
                raise CompilerError(ERR_BAD_SYSCALL_ARGS, node)

            r, g, b = map(self.as_value, node.args)
            self.ins_append(f'draw clear {r} {g} {b}')

        elif method == 'color':
            if len(node.args) == 3:
                r, g, b, a = *map(self.as_value, node.args), 255
            elif len(node.args) == 4:
                r, g, b, a = map(self.as_value, node.args)
            else:
                raise CompilerError(ERR_BAD_SYSCALL_ARGS, node)

            self.ins_append(f'draw color {r} {g} {b} {a}')

        elif method == 'stroke':
            if len(node.args) != 1:
                raise CompilerError(ERR_BAD_SYSCALL_ARGS, node)

            width = self.as_value(node.args[0])
            self.ins_append(f'draw stroke {width}')

        elif method == 'line':
            if len(node.args) != 4:
                raise CompilerError(ERR_BAD_SYSCALL_ARGS, node)

            x0, y0, x1, y1 = map(self.as_value, node.args)
            self.ins_append(f'draw line {x0} {y0} {x1} {y1}')

        elif method == 'rect':
            if len(node.args) != 4:
                raise CompilerError(ERR_BAD_SYSCALL_ARGS, node)

            x, y, width, height = map(self.as_value, node.args)
            self.ins_append(f'draw rect {x} {y} {width} {height}')

        elif method == 'hollow_rect':
            if len(node.args) != 4:
                raise CompilerError(ERR_BAD_SYSCALL_ARGS, node)

            x, y, width, height = map(self.as_value, node.args)
            self.ins_append(f'draw lineRect {x} {y} {width} {height}')

        elif method == 'poly':
            if len(node.args) == 4:
                x, y, radius, sides, rotation = *map(self.as_value, node.args), 0
            elif len(node.args) == 5:
                x, y, radius, sides, rotation = map(self.as_value, node.args)
            else:
                raise CompilerError(ERR_BAD_SYSCALL_ARGS, node)

            self.ins_append(f'draw poly {x} {y} {sides} {radius} {rotation}')

        elif method == 'hollow_poly':
            if len(node.args) == 4:
                x, y, radius, sides, rotation = *map(self.as_value, node.args), 0
            elif len(node.args) == 5:
                x, y, radius, sides, rotation = map(self.as_value, node.args)
            else:
                raise CompilerError(ERR_BAD_SYSCALL_ARGS, node)

            self.ins_append(f'draw linePoly {x} {y} {sides} {radius} {rotation}')

        elif method == 'triangle':
            if len(node.args) != 6:
                raise CompilerError(ERR_BAD_SYSCALL_ARGS, node)

            x0, y0, x1, y1, x2, y2 = map(self.as_value, node.args)
            self.ins_append(f'draw triangle {x0} {y0} {x1} {y1} {x2} {y2}')

        # elif method == 'image':
        #     pass

        elif method == 'flush':
            if len(node.args) == 0:
                self.ins_append(f'drawflush display1')
            elif len(node.args) == 1:
                value = self.as_value(node.args[0])
                self.ins_append(f'drawflush {value}')
            else:
                raise CompilerError(ERR_BAD_SYSCALL_ARGS, node)

        else:
            raise CompilerError(ERR_UNSUPPORTED_SYSCALL, node)

    def emit_control_syscall(self, node: ast.Call):
        """This will be called by visit_Expr when it encounters a Control(...) command."""

        # TODO may want to reimplement much of this in a more object-oriented fashion, e.g., ripple1.shoot(...)
        #      rather than Control.shoot( ripple1, ... )

        method = node.func.attr
        if method == 'enabled':
            if len(node.args) != 2:
                raise CompilerError(ERR_BAD_SYSCALL_ARGS, node)

            link, enabled = map(self.as_value, node.args)
            self.ins_append(f'control enabled {link} {enabled}')
        elif method == 'shoot':
            if len(node.args) == 3:
                link, x, y, enabled = *map(self.as_value, node.args), 1
            elif len(node.args) == 4:
                link, x, y, enabled = map(self.as_value, node.args)
            else:
                raise CompilerError(ERR_BAD_SYSCALL_ARGS, node)

            self.ins_append(f'control shoot {link} {x} {y} {enabled}')
        elif method == 'ceasefire':
            if len(node.args) != 1:
                raise CompilerError(ERR_BAD_SYSCALL_ARGS, node)

            link = self.as_value(node.args[0])
            self.ins_append(f'control shoot {link} 0 0 0')
        else:
            raise CompilerError(ERR_UNSUPPORTED_SYSCALL, node)

    def as_target(self, node):
        """This will be called by various other methods when they encounter some node that specifies something target
           variable that they need to assign a value to.  This returns that target as a string."""
        if not isinstance(node, ast.Name):
            raise CompilerError(ERR_COMPLEX_ASSIGN, node)
            # TODO This rules out x,y = 30,50 (for which targets[0] will be the tuple (x,y) )  May want to support that?
            # TODO Take care with how this will interact with vectorized operations
        return self.as_name(node.id)

    def as_name(self, name:str)->str:
        """This takes a variable name, wraps it in {} if it is an arg in a function def to prepare it for
           mangling/substitution, and maps it to its mlog @equivalent if it is a special variable. """
        if self._in_def:
            if name in self._in_def.args: name = '{'+name+'}' # wrap fn args in braces for substitution
            #TODO if you want to mangle other local variables used in a function (not just args), the way to do that
            #     would be to wrap them in {} here, store a list of such local args on the Function,
            #     and change Function.mapped_ins() to include mangling for local variables in its default map

        if name in AT_VARIABLES:  # e.g., Unit -> @unit,  This -> @this
            name = AT_VARIABLES[name]

        return name

    def as_value(self, node, allow_dummy = True):
        """This will be called by various other methods when they encounter some node that they'll need to treat as an
           atomic expression in mlog code, e.g. for both y and 3 in x=y+3.  This coerces the given node to a string that
           can appear in mlog code.  In the case where node is itself complex (e.g., the y*2 in x = 1 + y*2), this
           first creates one or more instructions to assign a dummy variable to the value in node (roughly dummy = y*2)
           and then returns that dummy for inclusion in whatever instruction needed an atomic way of referring to this
           complex value.
           This method first tries to see if node is anything it knows how to turn into an atomic expression,
           e.g., a constant (like 1 or "yarn"), a variable name (like x), or an idiomatic expressions for special mlog
           variables like @unit and @this.  Anything else is assumed to be some sort of complex expression that
           will take additional instruction(s) to turn into something atomic, so a new dummy variable will be created
           to store the value of this complex expression, so that we can return this dummy variable to serve as an
           atom in some larger expression, which is what as_value was called upon to do.  We then pass the buck
           (often back) to create_assign to figure out how to assign the right value to this dummy.
           This use of dummy variables will be disabled when allow_dummy is not True (e.g. when create_assign is
           trying to fill in the right hand side of x = ___), which can prevent infinite loops that could arise when
           neither create_assign nor as_value knows a good way to approach some node. In that case an error is raised.
           """

        if isinstance(node, ast.Constant):  # constants include True, 1.5, and "yarn"
            if isinstance(node.value, bool):
                return ('false', 'true')[node.value]
            elif isinstance(node.value, (int, float)):
                return str(node.value)
            elif isinstance(node.value, str):
                return '"' + ''.join(c for c in node.value if c >= ' ' and c != '"') + '"' #scrub quotes and control characters from string
            else:
                raise CompilerError(ERR_COMPLEX_VALUE, node)

        elif isinstance(node, ast.Name): # variable names
            assert isinstance(node.ctx, ast.Load)
            # TODO should this raise a CompilerError like most other errors do?
            #      I guess the plan is that as_value won't get called for variables in the .Store context (e.g.,
            #      the x in x = y) so this assertion would just serve to warn us that we'd called as_value at a time
            #      that we hadn't meant to, so the error is probably ours and not the users, so no compiler error?
            #      Should probably just remove this once we're confident our code works?
            return self.as_name(node.id)

        # most calls will be handled in create_Assign. But Env.methods compile to @variables, so handled here.
        elif ( isinstance(node, ast.Call) and
               isinstance(node.func, ast.Attribute) and
               node.func.value.id == 'Env'):
            return self.env_as_value(node.func) # map Env.methods to variables, e.g Env.this() -> @this

        # TODO if we want any Env.attributes (or other idiomatic object.attribute expressions that compile to atomic
        #      mlog expressions) we'd need to catch them here

        elif allow_dummy:
            temp_variable = self.Dummy("temp")
            #TODO it would aid readability of compiled code if we somehow set things up to make more informative names
            #     for temp variables, e.g., using __sum23 to store the output of an addition op
            #     One way of doing this would be to task CreateAssign with coming up with the temp variable at the
            #     last moment, and returning it here

            self.create_Assign(temp_variable, node)
            return temp_variable
        else:
            raise CompilerError(ERR_COMPLEX_VALUE, node)

    def env_as_value(self, node):
        """This is used to process expressions like Env.width, which compile to mlog globals like @mapw """
        var = ENV_TO_VAR.get(node.attr)
        if var is None:
            raise CompilerError(ERR_UNSUPPORTED_SYSCALL, node)
        return var

    def generate_masm(self):
        """This will be called after the full list self._main of mlog instructions is generated and optimized.
           This concatenates these together into a single string of mlog assembly (masm) code, and substitutes in
           finalized line numbers for the various jump labels."""

        if any(fn.mode != "inline" for fn in self._functions.values()):
            # TODO could trim this instruction if preceding line is end or jump...always
            self.ins_append("jump 0 always") # jump back to beginning since all that's left is function def bodies

        for fn in self._functions.values():  # append the body of each jumpy function to the end of the program
            if fn.mode != "inline":
                self.ins_append(fn.mapped_ins())

        n = 0                      # current line number
        labels_to_linenumbers = {} # will map each used label name to the linenumber of its jump destination
        for i in self._main:  # assign finalized line numbers to all instructions/labels
            # i.linenumber = n # not actually needed yet, and would cause errors if any non-Instruction str's in ._main
            if not i and i.label: labels_to_linenumbers[i.label.name] = n # remember what number to map this label to
            if i: n+=1  # unless i was an empty no-op instruction (a label destination), increment line number count

        # TODO I've updated most additions to ._main to append actual Instructions, rather than mere strings,
        #      but not quite all.  The main exceptions are old ad hoc substitutions of line numbers into strings,
        #      which end up replacing actual Instructions with mere strings.  These should probably be reworked to
        #      use the new label system for line-number substitution (and will likely *have* to be reworked this way
        #      before we implement any sort of optimization pass that changes line numbers).  In the meantime, I've
        #      written this so that it won't crash even if some mere strings clog up our Instructions conveyer belt. :-)

        if n > MAX_INSTRUCTIONS:
            raise CompilerError(ERR_TOO_LONG, ast.Module(lineno=0, col_offset=0))

        print("\n---- Readable nearly-compiled code with jump labels -------------------------")
        for i in self._main:
            print( "  ", i.__repr__() )
        print("-----------------------------------------------------------------------------")

        # concatenate non-empty instructions together into a long string broken by newline characters
        # at this point, jump instructions will still contain {labels} rather than actual linenumbers
        masm_with_labels = '\n'.join(i for i in self._main if i)

        #TODO it's not at all clear that you really want an explicit 'end' here!  My understanding is that
        #   'end' wipes all variables, whereas simply reaching the end with no explicit 'end' command instead
        #   loops back to the beginning, retaining the values of variables.  The choice of these should be left up
        #   to users, not forced by Pyndustric.  end() should probably instead be included as an explicit command
        #   that users might opt to include when they want to restart with variable wipe.
        #Telos: note I eliminated appending '\nend\n' since not all programs will want it!

        #TODO check what happens if a program concludes with a label, and hence has jump instruction go past last line?

        # replace all jump instruction {labels} with their destination linenumbers
        masm_with_linenumbers = masm_with_labels.format_map( labels_to_linenumbers )

        return masm_with_linenumbers

