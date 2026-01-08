/-!
# Random Number Generation for Property Testing

Provides a simple LCG-based random number generator and the Gen monad
for composable random value generation.
-/

namespace Crucible.Property

/-- Random generator state using Linear Congruential Generator algorithm. -/
structure RandState where
  seed : UInt64
  deriving Inhabited, Repr

namespace RandState

/-- Create state from a seed value. -/
def fromNat (seed : Nat) : RandState :=
  ⟨UInt64.ofNat seed⟩

/-- Create state from current time (for non-deterministic testing). -/
def fromTime : IO RandState := do
  let nanos ← IO.monoNanosNow
  pure ⟨UInt64.ofNat nanos⟩

/-- Get next random UInt64 and updated state using LCG algorithm. -/
def next (r : RandState) : UInt64 × RandState :=
  -- LCG parameters from PCG family
  let a : UInt64 := 6364136223846793005
  let c : UInt64 := 1442695040888963407
  let next := a * r.seed + c
  (next, ⟨next⟩)

/-- Get random Nat in range [lo, hi]. -/
def nextNat (r : RandState) (lo hi : Nat) : Nat × RandState :=
  let (raw, r') := r.next
  let range := hi - lo + 1
  if range == 0 then (lo, r')
  else
    let n := raw.toNat % range + lo
    (n, r')

/-- Get random Int in range [lo, hi]. -/
def nextInt (r : RandState) (lo hi : Int) : Int × RandState :=
  let (raw, r') := r.next
  let range := hi - lo + 1
  if range <= 0 then (lo, r')
  else
    let n := Int.ofNat (raw.toNat % range.toNat) + lo
    (n, r')

/-- Get random Float in range [0, 1). -/
def nextFloat (r : RandState) : Float × RandState :=
  let (raw, r') := r.next
  let f := raw.toNat.toFloat / UInt64.size.toFloat
  (f, r')

/-- Get random Bool with 50% probability. -/
def nextBool (r : RandState) : Bool × RandState :=
  let (raw, r') := r.next
  (raw.toNat % 2 == 0, r')

/-- Split state into two independent streams. -/
def split (r : RandState) : RandState × RandState :=
  let (v1, r1) := r.next
  let (v2, _) := r1.next
  (⟨v1⟩, ⟨v2 ^^^ 0x9E3779B97F4A7C15⟩)

end RandState


/-- Generator monad: produces random values with size control.

The size parameter allows generators to produce larger values as testing
progresses. Early tests use small values (fast, find simple bugs), later
tests use larger values (more thorough coverage). -/
structure Gen (α : Type u) where
  /-- Run the generator with random state and size parameter. -/
  run : RandState → Nat → α × RandState

namespace Gen

/-- Map a function over generated values. -/
@[inline]
protected def map {α β : Type u} (f : α → β) (g : Gen α) : Gen β :=
  ⟨fun r size =>
    let (a, r') := g.run r size
    (f a, r')⟩

/-- Pure value (constant generator). -/
@[inline]
protected def pure {α : Type u} (a : α) : Gen α :=
  ⟨fun r _ => (a, r)⟩

/-- Sequential composition of generators. -/
@[inline]
protected def bind {α β : Type u} (g : Gen α) (f : α → Gen β) : Gen β :=
  ⟨fun r size =>
    let (a, r') := g.run r size
    (f a).run r' size⟩

/-- Sequencing generators. -/
@[inline]
protected def seq {α β : Type u} (gf : Gen (α → β)) (ga : Unit → Gen α) : Gen β :=
  Gen.bind gf fun f => Gen.map f (ga ())

instance : Functor Gen where
  map := Gen.map

instance : Pure Gen where
  pure := Gen.pure

instance : Bind Gen where
  bind := Gen.bind

instance : Seq Gen where
  seq := Gen.seq

instance : Applicative Gen where
  pure := Gen.pure
  seq := Gen.seq

instance : Monad Gen := {}

/-- Get the current size parameter. -/
def getSize : Gen Nat :=
  ⟨fun r size => (size, r)⟩

/-- Run generator with modified size. -/
def resize (f : Nat → Nat) (g : Gen α) : Gen α :=
  ⟨fun r size => g.run r (f size)⟩

/-- Run generator with size computed from current size. -/
def sized (f : Nat → Gen α) : Gen α :=
  ⟨fun r size => (f size).run r size⟩

/-- Generate random Nat uniformly from range [lo, hi]. -/
def choose (lo hi : Nat) : Gen Nat :=
  ⟨fun r _ => r.nextNat lo hi⟩

/-- Generate random Int uniformly from range [lo, hi]. -/
def chooseInt (lo hi : Int) : Gen Int :=
  ⟨fun r _ => r.nextInt lo hi⟩

/-- Generate random Float in [0, 1). -/
def float01 : Gen Float :=
  ⟨fun r _ => r.nextFloat⟩

/-- Generate random Float in range [lo, hi). -/
def floatRange (lo hi : Float) : Gen Float := do
  let f ← float01
  pure (lo + f * (hi - lo))

/-- Generate random Bool. -/
def bool : Gen Bool :=
  ⟨fun r _ => r.nextBool⟩

/-- Execute generator with given seed and size, returning value and final state. -/
def run' (g : Gen α) (seed : Nat) (size : Nat) : α × RandState :=
  g.run (RandState.fromNat seed) size

/-- Execute generator and return only the value. -/
def sample (g : Gen α) (seed : Nat) (size : Nat := 100) : α :=
  (g.run' seed size).1

/-- Generate a list of n samples with different states. -/
def sampleMany (g : Gen α) (seed : Nat) (n : Nat) (size : Nat := 100) : List α :=
  let rec go (r : RandState) (remaining : Nat) (acc : List α) : List α :=
    match remaining with
    | 0 => acc.reverse
    | n + 1 =>
      let (a, r') := g.run r size
      go r' n (a :: acc)
  go (RandState.fromNat seed) n []

end Gen

end Crucible.Property
