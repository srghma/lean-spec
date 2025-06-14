import Colorized

namespace Spec

open Colorized (Colorized Color Style)

/-- A `Style` is a list of color or text style modifiers. -/
inductive StyleModifier
  | fg (c : Color)
  | bg (c : Color)
  | style (s : Style)

namespace Style

/-- Apply a list of `Modifier`s to a string. -/
@[inline] def styled (mods : List StyleModifier) (str : String) : String :=
  let apply : StyleModifier → String → String
    | .fg c, s => Colorized.bgColor c s
    | .bg c, s => Colorized.color c s
    | .style sty, s => Colorized.style sty s
  mods.foldr apply str

@[inline] def red     : List StyleModifier := [.fg Color.Red]
@[inline] def green   : List StyleModifier := [.fg Color.Green]
@[inline] def yellow  : List StyleModifier := [.fg Color.Yellow]
@[inline] def cyan    : List StyleModifier := [.fg Color.Cyan]
@[inline] def magenta : List StyleModifier := [.fg Color.Magenta]
@[inline] def dim     : List StyleModifier := [.style Style.Faint]
@[inline] def bold    : List StyleModifier := [.style Style.Bold]

/-- Indent by `i` spaces. -/
@[inline] def indent (i : Nat) : String := ⟨List.replicate i ' '⟩
