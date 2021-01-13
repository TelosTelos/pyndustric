import ast # standard python module for generating and using an abstract syntax tree (ast) representing python code

from dataclasses import dataclass # standard python module for simple classes used just to store data in attributes
from collections import Counter   # dictionary subclass used to count instances of different types of label/variable
import inspect                    # standard python module used to retrieve source code from functions to compile them
import textwrap                   # standard python module used for changing indentation, e.g., of source code
from pathlib import Path           # standard python module for handling filenames to load compilable code from file
from typing import Callable, Union # used for type-checking / debugging

from .constants import *          # definitions of various constants mapping Python entitites to mlog equivalents

@dataclass
class Function:
    """This stores information about a user-defined function that is callable within a compilable program."""
    start: int   #  Line number for the start of the function within the compiled program
    argc: int    #  Count of number of arguments the function takes

class _Label(str):
    """This subclass of str stores information about a label that can be used to mark a destination for mlog jump
       commands. Each label itself is a string of the form "{labelname}", which is how it will appear in instructions
       until eventually being replaced with an actual line number.
       During its creation, each label creates a .destination no-op instruction, which can be included in a
       compilers ._ins list of instructions as a placeholder to mark the destination for jumps involving this label.
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
       in compiled mindustry assembly (masm) code.  Instructions will be listed in a compiler object's ._ins
       attribute, until eventually being concatenated to make output compiled code.  An instruction created
       via Instruction( some_Label ) will be a no-op empty string, but will remember that Label as self.label .
       Each non-empty Instruction should be a valid mlog instruction *except* that line number destinations for jump
       instructions should instead have something of the form {labelname} in place of the line number, which
       will be replaced with an actual line number in the finalized output."""

    linenumber: int  #  Line number for this in the final output (or for next op instruction if this is no_op)
    label: _Label    #  Indicates what label if any, marks this instruction as its destination

    def __new__(cls, content ):
        if isinstance( content, _Label ):
            i = super().__new__(cls, "")
            i.label = content
        else:
            i = super().__new__(cls, content)
            i.label = None
        return i

class CompilerError(ValueError):
    def __init__(self, code, node: ast.AST):
        super().__init__(f'[{code}/{node.lineno}:{node.col_offset}] {ERROR_DESCRIPTIONS[code]}')


class Compiler(ast.NodeVisitor):
    """ast.NodeVisitor is a superclass with methods that enable straightforward visitation of the various nodes
    in an AST syntax tree, calling corresponding methods for the different types of nodes as they're visited, where
    parent nodes typically opt to initiate visits for their children.  This subclass overwrites the default visit
    methods with ones that generate compiled mlog code as the various parts of the tree are visited."""
    def __init__(self):
        # self._ins will end up being a list of instructions that will compose the compiled code.
        # we begin with an instruction to initialize the stack
        self._ins = [ Instruction( f'set {REG_STACK} 0') ]
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
        """This appends to this compiler's list ._ins of instructions an Instruction with content content."""
        if not isinstance( content, Instruction ): content = Instruction(content) # coerce content to instruction
        self._ins.append( content )

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
        target = node.targets[0] # the variable we'll assign a value to, e.g., the x in "x = 1"
        if not isinstance(target, ast.Name):
            raise CompilerError(ERR_COMPLEX_ASSIGN, node)
            # TODO This rules out x,y = 30,50 (for which targets[0] will be the tuple (x,y) )  May want to support that?
            # TODO Take care with how this will interact with vectorized operations

        self.create_Assign( target.id, node.value ) # Create instruction(s) to set target = value

        if len(node.targets) > 1: # if this was a multi-assignment like x = y = 0
            # TODO ERR_MULTI_ASSIGN was raised here, but now this compiles fine. Could delete this error + its test.
            for additional_target in node.targets[1:]:
                self.ins_append(f"set {additional_target.id} {target.id}") # set the later targets to mimic first

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

        print(f"\nCreating assignment instruction(s) for {variable} = {ast.dump(newvalue)}")

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

                    if len(newvalue.args) != fn.argc:
                        raise CompilerError(ERR_ARGC_MISMATCH, node)

                    for arg in newvalue.args:
                        val = self.as_value(arg)
                        self.ins_append(f'write {val} cell1 {REG_STACK}')
                        self.ins_append(f'op add {REG_STACK} {REG_STACK} 1')
                    # TODO  probably better to set the values straight into the relevant variables, rather than slowly piping through stack

                    # `REG_STACK` is not updated because it's immediately read by the function.
                    # If it was, the `op add` followed by the `op sub` would be redundant.
                    self.ins_append(f'write @counter cell1 {REG_STACK}')
                    self.ins_append(f'jump {fn.start} always')
                    return REG_RET

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
        print(f"\nUsing keyword args: {kwargs}" )
        kwargs['order'] = RADAR_ORDERS.get( kwargs['order'] ) # order can accept unusual values like min, max
        if kwargs['order'] is None:
            raise CompilerError(ERR_UNSUPPORTED_EXPR, newvalue )

        if kwargs['order'] in ['min','max']:  # we allow min and max as order args, map to 1,0
            kwargs['order'] = 1 if kwargs['order'] == 'min' else 0
        self.ins_append(f"{radar} {criteria} {kwargs['key']} {obj} {kwargs['order']} {variable}")

    def visit_AugAssign(self, node: ast.Assign):
        """This will be called for any* augmenting assignment statement, like "x += 1"  """
        target = node.target # e.g., x in "x += 1"
        if not isinstance(target, ast.Name):
            raise CompilerError(ERR_COMPLEX_ASSIGN, node)

        op = BIN_OPS.get(type(node.op))
        if op is None:
            raise CompilerError(ERR_UNSUPPORTED_OP, node)

        right = self.as_value(node.value)
        self.ins_append(f'op {op} {target.id} {target.id} {right}')
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

        target = node.target
        if not isinstance(target, ast.Name):
            raise CompilerError(ERR_COMPLEX_ASSIGN, node)
            # TODO: again the problem isn't that "for 12 in range(5):" is *complex*, it's that 12 isn't the right sort of thing to assign values to

        call = node.iter
        if not isinstance(call, ast.Call):
            raise CompilerError(ERR_UNSUPPORTED_ITER, node)

        inject = []

        if isinstance(call.func, ast.Attribute) \
                and call.func.value.id == 'Env' and call.func.attr == 'links':
            it = REG_IT_FMT.format(call.lineno, call.col_offset)
            start, end, step = 0, '@links', 1
            inject.append(f'getlink {target.id} {it}')
        elif isinstance(call.func, ast.Name) and call.func.id == 'range':
            it = target.id
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

        self.ins_append(f'set {it} {start}')

        self.ins_append(f'jump {{}} greaterThanEq {it} {end}')
        # TODO  if the step in range(start,stop,step) is negative, the relevant test is lessThanEq
        condition = len(self._ins) - 1

        self._ins.extend(inject)
        for subnode in node.body:
            self.visit(subnode)

        self.ins_append(f'op add {it} {it} {step}')
        self.ins_append(f'jump {condition} always')
        self._ins[condition] = self._ins[condition].format(len(self._ins))

    def visit_FunctionDef(self, node):
        """This will be called for any* function definition of the form def fname( args ): body """

        # TODO forbid recursion (or implement it by storing and restoring everything from stack)
        #      Telos: There's no need to actively forbid recursion, though it may be good to warn about dangers of it.
        #             So long as there is no stack to overflow, recursion can work perfectly fine as a form of infinite
        #             loop, or the recursion can be escaped with an end() command, a jump_to( label ) command, or simply
        #             by entering into some other infinite loop and never return-ing. The key point is just to warn that
        #             recursive calls will overwrite the value of the return pointer (if we follow my suggestion of
        #             implementing that in a dummy variable to forego needing a stack) and thereby lose track of
        #             whatever it was that called the function in the first place.  If you instead push return addresses
        #             onto a stack, and wait to pop them at return-time, then recursion will still be fine, and will
        #             even return all the way back to the first caller, with the only caveat being that all variables
        #             are global, effectively including function arguments, so you have to be a bit cautious how
        #             you write the code.
        # TODO local variable namespace per-function
        #      Telos:  I doubt that mindustry's processor-speed is fast enough to really make this worthwhile.
        #              I'd recommend instead just documenting that mlog uses a single global namespace, and that
        #              each call to a function overwrites the prior global values of its arguments.

        if self._in_def is not None:
            raise CompilerError(ERR_NESTED_DEF, node)
        # TODO there's no reason we *couldn't* allow nested function defns, though it may be more tricky than it's worth
        #      At first blush, I think these actually *probably* would work as is, if you remove this test banning them.
        #      The main uses of nested function definitions involve namespaces which probly aren't worth bothering with.
        #      Another use is to assign a function name to a different function definition, depending upon which
        #      def(s) end up being executed. The best way to implement that would probably be to assign a variable for
        #      each function name that points to the line number for whatever the current definition of that function
        #      is, and rather than compiling def simply to jump past the body of the function, each time a def is
        #      encountered, it should first change that function's name to point to this def's body, rather than
        #      whatever other def of the same-named function it might previously have pointed to.  I.e., this would treat
        #      function names as Python does, just as variables that point to an object that happens to be callable.
        #      In some ways, this would be better than your unPythonic assumption that each function name can have only
        #      one def in a program, though it would cost an instruction-cycle each time a def is "executed" which is
        #      a pretty minor cost.  (Regardless, we probably should make an unPythonic assumption that a given
        #      function-name will always accept the same arguments, though I suppose if we keep pushing positional args
        #      on the stack to pass them, then we needn't even assume that!)

        if node.name in self._functions or node.name == 'print':
            raise CompilerError(ERR_REDEF, node)

        self._in_def = node.name
        reg_ret = f'{REG_RET_COUNTER_PREFIX}{len(self._functions)}'

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
        self.ins_append('jump {} always')

        prologue = len(self._ins)  # line number where the function definition begins
        self._functions[node.name] = Function(start=prologue, argc=len(args.args))

        self.ins_append(f'read {reg_ret} cell1 {REG_STACK}')
        for arg in args.args:
            self.ins_append(f'op sub {REG_STACK} {REG_STACK} 1')
            self.ins_append(f'read {arg.arg} cell1 {REG_STACK}')

        # TODO argument passing would be more efficient if function-calling simply stored the arguments into the relevant
        #      variables directly, rather than slowly piping them through a memory block, and this would also obviate
        #      the need for an associated memory block.  (This would *not* however allow for recursion to retain
        #      different namespaces for different levels, but to allow that we'd also need to store old values for the
        #      namespace in the stack too, which would be really taxing for slow mindustry processors!)

        for subnode in node.body:
            self.visit(subnode)

        epilogue = len(self._ins)
        # TODO use the new label system for these substitutions

        for i in range(prologue, epilogue):
            if '{epilogue}' in self._ins[i]:
                self._ins[i] = self._ins[i].format(epilogue=epilogue)

        # Add 1 to the return value to skip the jump that made the call.
        self.ins_append(f'op add @counter {reg_ret} 1')

        end = len(self._ins)
        self._ins[prologue - 1] = self._ins[prologue - 1].format(end)
        self._in_def = None

    def visit_Return(self, node):
        """This will be called for any instance of Return, typically from within a function definition."""
        val = self.as_value(node.value)
        self.ins_append(f'set {REG_RET} {val}')
        self.ins_append('jump {epilogue} always')
        # TODO  better to pass back the return value in a dummy variable, rather than requiring a stack
        # TODO  maybe raise an error if not self._in_def  (can't return except when within a function definition)
        #       what we have currently will end up leaving the {epilogue} in the compiled program
        #       Or does AST itself enforce that Return must be within the scope of a Def?

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

        elif isinstance(node, ast.Name):
            assert isinstance(node.ctx, ast.Load)
            # TODO should this raise a CompilerError like most other errors do?
            #      I guess the plan is that as_value won't get called for variables in the .Store context (e.g.,
            #      the x in x = y) so this assertion would just serve to warn us that we'd called as_value at a time
            #      that we hadn't meant to, so the error is probably ours and not the users, so no compiler error?
            #      Should probably just remove this once we're confident our code works?

            #TODO capture special variable names like Unit and This and map them to their mlog equivs @unit, @this

            return node.id

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
        """This will be called after the full list self._ins of mlog instructions is generated and optimized.
           This concatenates these together into a single string of mlog assembly (masm) code, and substitutes in
           finalized line numbers for the various jump labels."""

        n = 0                      # current line number
        labels_to_linenumbers = {} # will map each used label name to the linenumber of its jump destination
        for i in self._ins:  # assign finalized line numbers to all instructions/labels
            # i.linenumber = n # not actually needed yet, and would cause errors if any non-Instruction str's in ._ins
            if not i and i.label: labels_to_linenumbers[i.label.name] = n # remember what number to map this label to
            if i: n+=1  # unless i was an empty no-op instruction (a label destination), increment line number count

        # TODO I've updated most additions to ._ins to append actual Instructions, rather than mere strings,
        #      but not quite all.  The main exceptions are old ad hoc substitutions of line numbers into strings,
        #      which end up replacing actual Instructions with mere strings.  These should probably be reworked to
        #      use the new label system for line-number substitution (and will likely *have* to be reworked this way
        #      before we implement any sort of optimization pass that changes line numbers).  In the meantime, I've
        #      written this so that it won't crash even if some mere strings clog up our Instructions conveyer belt. :-)

        if n > MAX_INSTRUCTIONS:
            raise CompilerError(ERR_TOO_LONG, ast.Module(lineno=0, col_offset=0))

        # concatenate non-empty instructions together into a long string broken by newline characters
        # at this point, jump instructions will still contain {labels} rather than actual linenumbers
        masm_with_labels = '\n'.join(i for i in self._ins if i)

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

