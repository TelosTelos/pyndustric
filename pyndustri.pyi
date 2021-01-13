from typing import Iterator, Optional, Callable, Union

class Link:
    """Represents a link."""


class Env:
    """
    Access to special environmental variables.
    """

    @staticmethod
    def this():
        """Return the `Building` object representing this logic processor."""

    @staticmethod
    def x():
        """Return the `x` coordinate where `this` is located."""

    @staticmethod
    def y():
        """Return the `y` coordinate where `this` is located."""

    @staticmethod
    def counter():
        """
        Return the Program Counter (also known as Instruction Pointer).

        The value is a number representing the index of the *next* instruction.
        """

    @staticmethod
    def links() -> Iterator[Link]:
        """Used to iterate over all the links."""

    @staticmethod
    def link_count():
        """Return how many links there are connected to `this` logic processor."""

    @staticmethod
    def time():
        """Return the current UNIX timestamp, in milliseconds."""

    @staticmethod
    def width():
        """Return the width of the entire map, in tiles."""

    @staticmethod
    def height():
        """Return the height of the entire map, in tiles."""


# https://github.com/Anuken/Mindustry/blob/e714d44/core/assets/bundles/bundle.properties#L979-L998
# https://github.com/Anuken/Mindustry/blob/8bc349b/core/src/mindustry/logic/LAccess.java#L6-L47
class Sensor:
    """
    Access to a property of a given link.
    """
    @staticmethod
    def copper(link: Link) -> int:
        """Amount of Copper stored or carried by the link."""
    @staticmethod
    def lead(link: Link) -> int:
        """Amount of Lead stored or carried by the link."""
    @staticmethod
    def coal(link: Link) -> int:
        """Amount of Coal stored or carried by the link."""
    @staticmethod
    def graphite(link: Link) -> int:
        """Amount of Graphite stored or carried by the link."""
    @staticmethod
    def titanium(link: Link) -> int:
        """Amount of Titanium stored or carried by the link."""
    @staticmethod
    def thorium(link: Link) -> int:
        """Amount of Thorium stored or carried by the link."""
    @staticmethod
    def silicon(link: Link) -> int:
        """Amount of Silicon stored or carried by the link."""
    @staticmethod
    def plastanium(link: Link) -> int:
        """Amount of Plastanium stored or carried by the link."""
    @staticmethod
    def phase_fabrix(link: Link) -> int:
        """Amount of Phase Fabric stored or carried by the link."""
    @staticmethod
    def surge_alloy(link: Link) -> int:
        """Amount of Surge Alloy stored or carried by the link."""
    @staticmethod
    def spore_pod(link: Link) -> int:
        """Amount of Spore Pod stored or carried by the link."""
    @staticmethod
    def sand(link: Link) -> int:
        """Amount of Sand stored or carried by the link."""
    @staticmethod
    def blast_compound(link: Link) -> int:
        """Amount of Blast Compound stored or carried by the link."""
    @staticmethod
    def pyratite(link: Link) -> int:
        """Amount of Pyratite stored or carried by the link."""
    @staticmethod
    def metaglass(link: Link) -> int:
        """Amount of Metaglass stored or carried by the link."""
    @staticmethod
    def scrap(link: Link) -> int:
        """Amount of Scrap stored or carried by the link."""
    @staticmethod
    def water(link: Link) -> int:
        """Amount of Water stored or carried by the link."""
    @staticmethod
    def slag(link: Link) -> int:
        """Amount of Slag stored or carried by the link."""
    @staticmethod
    def oil(link: Link) -> int:
        """Amount of Oil stored or carried by the link."""
    @staticmethod
    def cryofluid(link: Link) -> int:
        """Amount of Cryofluid stored or carried by the link."""
    @staticmethod
    def items(link: Link) -> int:
        """The sum of all items contained or carried by the link."""
    @staticmethod
    def first_item(link: Link) -> Optional[Link]:
        """A link to the first contained item carried by the link, if any."""
    @staticmethod
    def liquids(link: Link) -> int:
        """The sum of all liquids contained or carried by the link."""
    @staticmethod
    def power(link: Link) -> int:
        """The current total power consumption by the link."""
    @staticmethod
    def max_items(link: Link) -> int:
        """How maximum amount of items the link can contain or carry."""
    @staticmethod
    def max_liquids(link: Link) -> int:
        """How maximum amount of liquids the link can contain or carry."""
    @staticmethod
    def max_power(link: Link) -> int:
        """The maximum amount of power the link can store."""
    @staticmethod
    def power_stored(link: Link) -> int:
        """The amount of power stored in the link."""
    @staticmethod
    def power_capacity(link: Link) -> int:
        """The amount of power the link can store."""
    @staticmethod
    def power_in(link: Link) -> int:
        """The net amount of power goiong into the link."""
    @staticmethod
    def power_out(link: Link) -> int:
        """The net amount of power goiong out of the link."""
    @staticmethod
    def ammo(link: Link) -> int:
        """The current amount of ammunition the current link has."""
    @staticmethod
    def max_ammo(link: Link) -> int:
        """The maximum amount of ammunition the current link has."""
    @staticmethod
    def health(link: Link) -> int:
        """The current health of the link."""
    @staticmethod
    def max_health(link: Link) -> int:
        """The maximum health of the link."""
    @staticmethod
    def heat(link: Link) -> int:
        """The current heat of the link."""
    @staticmethod
    def efficiency(link: Link) -> int:
        """The current efficiency of the link (base and boosts)."""
    @staticmethod
    def rotation(link: Link) -> int:
        """The current rotation of the link."""
    @staticmethod
    def x(link: Link) -> int:
        """The X logic tile coordinate of the link. Whole numbers are at the center of the tile."""
    @staticmethod
    def y(link: Link) -> int:
        """The Y logic tile coordinate of the link. Whole numbers are at the center of the tile."""
    @staticmethod
    def shoot_x(link: Link) -> int:
        """The X coordinate where the link is shooting to."""
    @staticmethod
    def shoot_y(link: Link) -> int:
        """The Y coordinate where the link is shooting to."""
    @staticmethod
    def shooting(link: Link) -> bool:
        """True if the link is currently shooting."""
    @staticmethod
    def mine_x(link: Link) -> int:
        """The X coordinate where the link is currently mining at."""
    @staticmethod
    def mine_y(link: Link) -> int:
        """The Y coordinate where the link is currently mining at."""
    @staticmethod
    def mining(link: Link) -> bool:
        """True if the link is currently mining"""
    @staticmethod
    def team(link: Link) -> int:
        """The team identifier to which the link belongs."""
    @staticmethod
    def type(link: Link) -> int:
        """The type of the link."""
    @staticmethod
    def flag(link: Link) -> int:
        """The custom flag variable stored in the link."""
    @staticmethod
    def controlled(link: Link) -> bool:
        """Is this link being controlled?"""
    @staticmethod
    def commanded(link: Link) -> bool:
        """Is this link being commanded?"""
    @staticmethod
    def name(link: Link) -> int:
        """The name of the link."""
    @staticmethod
    def config(link: Link) -> bool:
        """The configuration of the link."""
    @staticmethod
    def payload(link: Link) -> int:
        """The amount of payload the link is carrying."""
    def payload_type(link: Link) -> int:
        """The type of the payload."""
    @staticmethod
    def enabled(link: Link) -> bool:
        """Is this link enabled?"""


class Control:
    """
    Lets you control a given link.
    """
    @staticmethod
    def enabled(link: Link, enabled: bool):
        """Sets the link enabled or disabled, based on the given value."""

    def shoot(link: Link, x: int, y: int, enabled: bool = True):
        """Sets the link to shoot or not, and if shooting, the position."""

    def ceasefire(link: Link):
        """Shorthand to stop firing."""


class Screen:
    """
    Method interface to the `draw` and `drawflush` command.
    """

    @staticmethod
    def clear(r: int, g: int, b: int):
        """Clear the entire display buffer to the given RGB values."""

    @staticmethod
    def color(r: int, g: int, b: int, a: int = 255):
        """Set the current brush color to the given RGBA values."""

    @staticmethod
    def stroke(width: int):
        """Set the current brush stroke width to the given value."""

    @staticmethod
    def line(x0: int, y0: int, x1: int, y1: int):
        """
        Draw a line from `(x0, y0)` towards `(x1, y1)`.

        The line will be as wide as previously defined by `stroke`, and the ends will be straight.

        You may use it to make rotated rectangles by setting a big enough stroke.
        """

    @staticmethod
    def rect(x: int, y: int, width: int, height: int):
        """
        Draw a filled rectangle with its bottom-left corner at `(x, y)` and size `(width, height)`.
        """

    @staticmethod
    def hollow_rect(x: int, y: int, width: int, height: int):
        """
        Like `rect`, but hollow. This border width considers the `stroke`.
        """

    @staticmethod
    def poly(x: int, y: int, radius: int, sides: int, rotation: int = 0):
        """
        Draw a polygon centered at `(x, y)` with the specified `radius` and `sides`.

        For example, 3 sides make a triangle, while a high number like 20 make it look like a circle.

        The rotation is optional and specified in degrees.
        """

    @staticmethod
    def hollow_poly(x: int, y: int, radius: int, sides: int, rotation: int = 0):
        """
        Like `poly`, but hollow. This border width considers the `stroke`.
        """

    @staticmethod
    def triangle(x0: int, y0: int, x1: int, y1: int, x2: int, y2: int):
        """
        Draw a triangle with corners at `(x0, y0)`, `(x1, y1)` and `(x2, y2)`.
        """

    @staticmethod
    def image(x: int, y: int, image: Resource, size: int, rotation: int = 0):
        """
        Draw the image resource centered at `(x, y)` with `size` and optional `rotation`.
        """

    @staticmethod
    def flush(display: str = None):
        """Flush the screen buffer to the display."""


def print(message: str, flush: Union[bool, str] = True):
    """
    Print a message. f-strings are supported and encouraged to do string formatting.

    `flush` may be a boolean indicating whether to emit `printflush` or not, or the name of the message.
    """


# -------------------------------------------------

class Vector:
    """A Vector is a very general superclass that is used to represent anything that has .x and .y coordinates,
    including Mindustry blocks, Mindustry units, and variables that are used to represent x,y coordinates.
    You can access any vector V's .x and .y components with V.x and V.y, though it will often be more convenient to
    use vectorized operations that operate on .x and .y together, just like vectorized operations on NumPy arrays.
    E.g., adding Vector(x1,y1) + Vector(x2,y2) yields Vector(x1+x2,y1+y2).  When a scalar is mathematically combined
    with a Vector, as in Vector(x1,y1)*2, that scalar is "broadcast" NumPy-style to interact with each component of
    the vector, yielding Vector(x1*2, y1*2). In Pyndustric, Vectors are the default arguments to give for any mlog
    function or operation that requires x and y components, e.g. dst( V ) returns the distance (or magnitude) of
    vector V, and Unit.move_to( V ) directs the currently bound Unit to approach the location specified by vector V.
    Pyndustric automatically interprets any list [x,y] as Vector(x,y) (since Mindustry's mlog does not provide
    any native operations involving lists)."""
    def __init__(self, x:float = 0, y:float = 0):
        self.x = x
        self.y = y

    def __add__( self, other: float|list|"Vector" ) -> "Vector":
        """This would be called whenever someone tries to add a vector + something else.  If a vector is added
        to another vector or to a two-element list, the x-components and y-components are added separately.
        If a vector is added to a scalar, the scalar is added to each component."""

    def __radd__(self, other: float|list|"Vector") -> "Vector":
        """This would be called whenever someone tries to something else + a vector.  If a vector is added
        to another vector or to a two-element list, the x-components and y-components are added separately.
        If a vector is added to a scalar, the scalar is added to each component."""

    def __sub__( self, other: float|list|"Vector" ) -> "Vector":
        """This would be called whenever someone tries to subtract a vector - something else.  If other is
        another vector or to a two-element list, the x-components and y-components are subtracted separately.
        If the other is a scalar, the scalar is subtracted from each component."""

    def __rsub__( self, other: float|list|"Vector" ) -> "Vector":
        """This would be called whenever someone tries to subtract something else - a vector.  If other is
        another vector or to a two-element list, the x-components and y-components are subtracted separately.
        If the other is a scalar, each component is subtracted from that scalar."""

    #TODO include dummy interface definitions for other vectorized overloaded operations

class SensableAttribute:
    """This is an attribute of some unit/block that can be read by an mlog sensor.
       In Pyndustric these can be referred to via object.attribute, e.g. vault1.copper or Unit.shooting.
       In Pyndustric, SensableAttribute names mirror those in Mindustry mlog, *except* Python does not
       use the leading @, and hyphens are transformed to underscores, e.g., @phase-fabric becomes .phase_fabric"""

class SensableObject(Vector):
    """This umbrella class can represent any block or unit that mlog sensor instructions can read attributes from.
       This is a subclass of Vector since all such objects have x,y locations that can participate in vectorized
       computations.  In Pyndustric, attributes of a SensableObject may be accessed as .attributes,
       e.g., vault1.copper or Unit.shooting """

    # TODO determine if any of the following attributes are unit-only or block-only, and if so move to subclass
    copper: SensableAttribute #  '@copper',
    lead: SensableAttribute #  '@lead',
    coal: SensableAttribute #  '@coal',
    graphite: SensableAttribute #  '@graphite',
    titanium: SensableAttribute #  '@titanium',
    thorium: SensableAttribute #  '@thorium',
    silicon: SensableAttribute #  '@silicon',
    plastanium: SensableAttribute #  '@plastanium',
    phase_fabric: SensableAttribute #  '@phase-fabric',
    surge_alloy: SensableAttribute #  '@surge-alloy',
    spore_pod: SensableAttribute #  '@spore-pod',
    sand: SensableAttribute #  '@sand',
    blast_compound: SensableAttribute #  '@blast-compound',
    pyratite: SensableAttribute #  '@pyratite',
    metaglass: SensableAttribute #  '@metaglass',
    scrap: SensableAttribute #  '@scrap',
    water: SensableAttribute #  '@water',
    slag: SensableAttribute #  '@slag',
    oil: SensableAttribute #  '@oil',
    cryofluid: SensableAttribute #  '@cryofluid',
    totalItems: SensableAttribute #  '@totalItems',
    firstItem: SensableAttribute #  '@firstItem',
    totalLiquids: SensableAttribute #  '@totalLiquids',
    totalPower: SensableAttribute #  '@totalPower',
    itemCapacity: SensableAttribute #  '@itemCapacity',
    liquidCapacity: SensableAttribute #  '@liquidCapacity',
    powerCapacity: SensableAttribute #  '@powerCapacity',
    powerNetStored: SensableAttribute #  '@powerNetStored',
    powerNetCapacity: SensableAttribute #  '@powerNetCapacity',
    powerNetIn: SensableAttribute #  '@powerNetIn',
    powerNetOut: SensableAttribute #  '@powerNetOut',
    ammo: SensableAttribute #  '@ammo',
    ammoCapacity: SensableAttribute #  '@ammoCapacity',
    health: SensableAttribute #  '@health',
    maxHealth: SensableAttribute #  '@maxHealth',
    heat: SensableAttribute #  '@heat',
    efficiency: SensableAttribute #  '@efficiency',
    rotation: SensableAttribute #  '@rotation',
    x: SensableAttribute #  '@x',   # should vectorize!
    y: SensableAttribute #  '@y',
    shootX: SensableAttribute #  '@shootX',   # should vectorize!
    shootY: SensableAttribute #  '@shootY',
    shooting: SensableAttribute #  '@shooting',
    mineX: SensableAttribute #  '@mineX', # should vectorize!
    mineY: SensableAttribute #  '@mineY',
    mining: SensableAttribute #  '@mining',
    team: SensableAttribute #  '@team',
    type: SensableAttribute #  '@type',
    flag: SensableAttribute #  '@flag',
    controlled: SensableAttribute #  '@controlled',
    commanded: SensableAttribute #  '@commanded',
    name: SensableAttribute #  '@name',
    config: SensableAttribute #  '@config',
    payload: SensableAttribute #  '@payloadCount',
    payloadType: SensableAttribute #  '@payloadType',
    enabled: SensableAttribute #  '@enabled',  # could treat as settable property?

    pos: Vector      # [@x , @y]
    shootPos: Vector # [@shootX, @shootY]
    minePos: Vector  # [@mineX, @mineY]

class RadarCriterion:
    """This is a criterion that any unit must meet to be returned by a radar method.
       The currently supported criteria are: any, enemy, ally, ground, flying, player, attacker, boss.  """

any = enemy = ally = ground = flying = player = attacker = boss = RadarCriterion()

class RadarKey:
    """This is a sorting criterion for determining which of multiple successful radar matches to return.
       The currently supported keys are:  distance, health, MaxHealth, armor, and shields.
       Note: setting radar order = min or = max will alter whether the minimal or maximal match will be returned."""

distance = health = maxHealth = armor = shields = RadarKey()


class UnitType:
    """This is used to represent any type of mindustry unit, e.g. manufactured units like mono or poly,
       or player-controlled units, like alpha, beta, or gamma.  A UnitType is used as an argument for
       Unit.bind_next( unittype ) to specify which type of unit you want the processor to bind/control."""

alpha = beta = gamma = flare = UnitType()
# TODO include remaining unit types

class AnyUnit( SensableObject ):
    """This is used to represent any mindustry unit, e.g. the currently bound unit, called Unit, previously bound
       units which have been stored as the value of a variable, or units detected by radar.
       As a subclass of Vector, this automatically inherits .x and .y coordinates, and various vectorized operations
       that will operate on this unit's location"""

    #TODO include dummy interface definitions for various unit attributes


class BoundUnit( AnyUnit ):
    """This is used to handle the mlog built-in variable @unit, which always represents the currently bound unit.
       Pyndustric compilable programs will typically refer to this as Unit.
       This is a subclass of AnyUnit and hence of Vector, so inherits the ability to refer to various attributes
       of the bound Unit, including its .x and .y coordinates (and allowing vectorized operations on those),
       and various other unit attributes.  In addition this subclass allows various methods for controlling the
       bound Unit."""

    def bind_next(self, unittype: UnitType):
        """In Pyndustric, unit binding can be done with Unit.bind_next( unittype ).
        This binds this processor to the next friedly unit of unittype.  Subsequent references to 'Unit' will
        refer to this unit, until a new unit is bound.  If your program loops back up to bind another unit,
        (e.g., by concluding and starting over), a new unit will be bound on the next iteration, so eventually
        you will have gone through all the units of unittype and automatically start over with the first.
        If you don't want all units of this type to do the same thing, you'll need to either (a) not iterate through
        all the units, and/or (b) refrain from giving the same commands to all the units you do iterate through."""

    def radar(self, criterion1:RadarCriterion, criterion2:RadarCriterion = any, criterion3:RadarCriterion = any,
              order:Union[Callable,bool,int]=min, key:RadarKey=distance )->AnyUnit:
        """This returns a unit within bound Unit's range meeting the given criteria (any, player, enemy, ally, attacker,
           ground, flying, boss), where the units are sorted by key (distance, health, maxHealth, armor, shields),
           and either the minimal or maximal match is returned, determined by whether order=min or order=max.
        """

    #TODO include dummy interface definitions for various methods for controlling bound Unit


Unit = BoundUnit()  # Pyndustric programs will refer mlog's @unit as Unit




class Block(SensableObject):
    """This represents a block located somewhere on the map, e.g., a turret, vault, memory processor, or conveyer belt.
       If your processor will be linked to blocks, like vault1 or ripple1, it is recommended that you declare these
       as arguments of type : Block for the function that you want to compile to be the mlog code for that processor.
       Blocks can also be found using TODO the following mlog functions
       In Pyndustric, attributes of a block may be accessed as .attributes,  e.g., vault1.copper """
    def __getitem__(self, index): pass         # You can read a slot of a memory block, e.g., with memory1[0]
    def __setitem__(self, index, value): pass  # You can write to a slot of a memory block, e.g., with memory1[0]=1

    def radar(self, criterion1:RadarCriterion, criterion2:RadarCriterion = any, criterion3:RadarCriterion = any,
              order:Union[Callable,bool,int]=min, key:RadarKey=distance ) -> AnyUnit:
        """This returns a unit within this block's range meeting the given criteria (any, player, enemy, ally, attacker,
           ground, flying, boss), where the units are sorted by key (distance, health, maxHealth, armor, shields),
           and either the minimal or maximal match is returned, determined by whether order=min or order=max.
        """

# ------------------------

def dst(vector_or_x: float|list|Vector, y:float = None ) -> float:
    """This built-in Mindustry mlog function computes the distance (or magnitude) along a 2D vector.
       In Pyndustric the argument can either be any Vector (including a unit or block) or separate
       specification of .x and .y components.  The distance between two objects, is the dst of their difference,
       e.g., the distance between the currently bound Unit and This processor is dst(Unit - This)."""

def label(name: str):
    """This marks a location in the code, so that jump commands can be used to move the point of execution here.
       This does not compile to an instruction, but instead is just is used to calculate which line-number
       should be used with jump commands."""

def jump( label: str):
    """This unconditionally jumps the flow of execution to the corresponding label, defined with label(name),
       using Mindustry's jump always command.  Be cautious using this with other flow-of-control mechanisms like
       functions or for or while loops. Mindustry's jump command allows conditional jumps (rather than jump always).
       To create conditional jumps in pyndustric, simply embed the jump command within a Python if statement, as this
       will compile to a mindustry conditional jump."""


