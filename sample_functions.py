
# These are sample functions, written in Python, whose contents can be compiled to mindustry mlog code.
# TODO not all of these are compilable yet, but the goal is to make them be!

# For debugging purposes, e.g., in PyCharm, it will be useful to declare arguments to the function
# corresponding to whatever mindustry blocks you expect to be linked to the processor running this code.
# These should typically be annotated as type Block.

from pyndustri import * # This interface module will help an IDE like PyCharm do auto-completion and error-checking

# ------------------------------------------------------------

def findPlayer(memory1: Block):
    """This compilable function finds the (or a) player and stores a reference to it in slot 1 of memory1.
    This is useful if other logic processors want to know where the player is, e.g., to direct units to
    follow the player."""
    Unit.bind_next( gamma )
    if Unit: jump("found it")
    Unit.bind_next( beta )
    if Unit: jump("found it")
    Unit.bind_next( alpha )
    label( "found it" )
    memory1[1] = Unit

def stayNearPlayer(memory1: Block):
    """This compilable function goes through all units of the type specified at beginning of loop, checks if they are
       more than maxD away from the player. If so it controls them to approach to approachD from the player, otherwise
       lets them continue path-finding on their own.  This presumes a reference to the player is stored in memory1[1],
       e.g by another processor running findPlayer. """
    maxD = 25       # maximum distance units are allowed to depart from player
    approachD = 20  # target distance from player units are ordered to approach
    while True:
        Unit.bind_next( flare )
        player = memory1[1]
        if dst( Unit - player ) > maxD:
            Unit.approach(player, approachD)
        else:
            Unit.pathfind()

def ammo_feeder( container1: Block, ripple1: Block ):
  """This simple compilable function flags an unflagged flare and has it forever ferry ammo from container1 to ripple1."""
  while not (Unit and Unit.flag == 0):
    Unit.bind_next(flare) # find a flare whose flag is 0
  Unit.flag = 1   # set its flag to 1, so other processors will know not to take control of it
  while Unit:  # so long as this unit survives...
    while dst( Unit - container1 ) > 5:
        Unit.move_to(container1)
    Unit.get_item(container1, graphite, 20)
    while dst( Unit - ripple1 ) > 5:
        Unit.move_to(ripple1)
    Unit.put_item(ripple1, graphite, 20)