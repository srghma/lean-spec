import Init.Data.String.Basic
import Init.Data.Char.Basic

-- TODO: endPos is expensive

@[inline] private def posOfSubstringAux (s : String) (pattern : String) (stopPos : String.Pos) (pos : String.Pos)
  (h_nonempty : pattern.endPos.byteIdx > 0) : Option String.Pos :=
  if h : pos.byteIdx + pattern.endPos.byteIdx > stopPos.byteIdx then
    none
  else
    if s.substrEq pos pattern 0 pattern.endPos.byteIdx then
      some pos
    else
      have h_advance : pos.byteIdx < (s.next pos).byteIdx := String.lt_next s pos
      have h_remaining : stopPos.byteIdx - (s.next pos).byteIdx < stopPos.byteIdx - pos.byteIdx := by
        omega
      posOfSubstringAux s pattern stopPos (s.next pos) h_nonempty
termination_by stopPos.byteIdx - pos.byteIdx

@[inline] def String.posOfSubstring? (s : String) (pattern : String) : Option String.Pos :=
  if h : pattern.isEmpty then
    some 0
  else
    have h_nonempty : pattern.endPos.byteIdx > 0 := by
      simp [String.isEmpty] at h
      cases Nat.eq_or_lt_of_le (Nat.zero_le pattern.endPos.byteIdx) with
      | inl h_eq =>
        simp [String.Pos.ext_iff] at h
        simp_all only [not_true_eq_false]
      | inr h_lt => exact h_lt
    posOfSubstringAux s pattern s.endPos 0 h_nonempty

#guard "hello world".posOfSubstring? "world" = .some ⟨6⟩
#guard "hello world world".posOfSubstring? "world" = .some ⟨6⟩
#guard "abcdefabcdef".posOfSubstring? "abc" = .some ⟨0⟩
#guard "abcdefabcdef".posOfSubstring? "xyz" = .none
#guard "abc".posOfSubstring? "" = .some ⟨0⟩
#guard "".posOfSubstring? "" = .some ⟨0⟩
#guard "".posOfSubstring? "a" = .none
#guard "🚀🌟🚀🌟".posOfSubstring? "🌟" = .some ⟨4⟩

@[inline] def String.containsSubstring (s : String) (pattern : String) : Bool :=
  (s.posOfSubstring? pattern).isSome

#guard "hello world".containsSubstring "world"
#guard !("hello world".containsSubstring "foo")
#guard "abcdef".containsSubstring "cde"
#guard !("abcdef".containsSubstring "xyz")
#guard "test".containsSubstring ""
#guard !("".containsSubstring "test")
#guard "🚀🌟".containsSubstring "🌟"

@[inline] private def lastPosOfSubstringAux (s : String) (pattern : String) (startPos : String.Pos)
  (h_nonempty : pattern.endPos.byteIdx > 0) : Option String.Pos :=
  if h : startPos.byteIdx < pattern.endPos.byteIdx then
    none
  else
    let checkPos := ⟨startPos.byteIdx - pattern.endPos.byteIdx⟩
    if s.substrEq checkPos pattern 0 pattern.endPos.byteIdx then
      some checkPos
    else
      if h_zero : startPos.byteIdx = 0 then
        none
      else
        let prevPos := s.prev startPos
        have h_prev : prevPos.byteIdx < startPos.byteIdx := by
          have h_pos : startPos ≠ 0 := by
            simp [String.Pos.ext_iff]
            simp_all only [gt_iff_lt, Nat.not_lt, not_false_eq_true]
          exact String.prev_lt_of_pos s startPos h_pos
        lastPosOfSubstringAux s pattern prevPos h_nonempty
termination_by startPos.byteIdx

@[inline] def String.lastPosOfSubstring? (s : String) (pattern : String) : Option String.Pos :=
  if h : pattern.isEmpty then
    some s.endPos
  else
    have h_nonempty : pattern.endPos.byteIdx > 0 := by
      simp [String.isEmpty] at h
      cases Nat.eq_or_lt_of_le (Nat.zero_le pattern.endPos.byteIdx) with
      | inl h_eq =>
        simp [String.Pos.ext_iff] at h
        simp_all only [not_true_eq_false]
      | inr h_lt => exact h_lt
    lastPosOfSubstringAux s pattern s.endPos h_nonempty

#guard "hello world".lastPosOfSubstring? "world" = .some ⟨6⟩
#guard "hello world world".lastPosOfSubstring? "world" = .some ⟨12⟩
#guard "abcdefabcdef".lastPosOfSubstring? "abc" = .some ⟨6⟩
#guard "abcdefabcdef".lastPosOfSubstring? "xyz" = .none
#guard "abc".lastPosOfSubstring? "" = .some ⟨3⟩
#guard "".lastPosOfSubstring? "" = .some ⟨0⟩
#guard "".lastPosOfSubstring? "a" = .none
#guard "🚀🌟🚀🌟".lastPosOfSubstring? "🌟" = .some ⟨12⟩

@[inline] private def findAllAux (s : String) (pattern : String) (stopPos : String.Pos) (pos : String.Pos)
  (h_nonempty : pattern.endPos.byteIdx > 0) (acc : List String.Pos) : List String.Pos :=
  if h : pos.byteIdx + pattern.endPos.byteIdx > stopPos.byteIdx then
    acc.reverse
  else
    if s.substrEq pos pattern 0 pattern.endPos.byteIdx then
      have h_advance : pos.byteIdx < (s.next pos).byteIdx := String.lt_next s pos
      have h_remaining : stopPos.byteIdx - (s.next pos).byteIdx < stopPos.byteIdx - pos.byteIdx := by
        omega
      findAllAux s pattern stopPos (s.next pos) h_nonempty (pos :: acc)
    else
      have h_advance : pos.byteIdx < (s.next pos).byteIdx := String.lt_next s pos
      have h_remaining : stopPos.byteIdx - (s.next pos).byteIdx < stopPos.byteIdx - pos.byteIdx := by
        omega
      findAllAux s pattern stopPos (s.next pos) h_nonempty acc
termination_by stopPos.byteIdx - pos.byteIdx

@[inline] def String.findAllPosOfSubstring (s : String) (pattern : String) : List String.Pos :=
  if h : pattern.isEmpty then
    []
  else
    have h_nonempty : pattern.endPos.byteIdx > 0 := by
      simp [String.isEmpty] at h
      cases Nat.eq_or_lt_of_le (Nat.zero_le pattern.endPos.byteIdx) with
      | inl h_eq =>
        simp [String.Pos.ext_iff] at h
        simp_all only [not_true_eq_false]
      | inr h_lt => exact h_lt
    findAllAux s pattern s.endPos 0 h_nonempty []

#guard "hello world world".findAllPosOfSubstring "world" = [⟨6⟩, ⟨12⟩]
#guard "abcdefabcdef".findAllPosOfSubstring "abc" = [⟨0⟩, ⟨6⟩]
#guard "aaaa".findAllPosOfSubstring "aa" = [⟨0⟩, ⟨1⟩, ⟨2⟩]
#guard "abcdef".findAllPosOfSubstring "xyz" = []
#guard "abc".findAllPosOfSubstring "" = []
#guard "".findAllPosOfSubstring "" = []
#guard "".findAllPosOfSubstring "a" = []
#guard "🚀🌟🚀🌟".findAllPosOfSubstring "🌟" = [⟨4⟩, ⟨12⟩]
#guard "hello hello hello".findAllPosOfSubstring "hello" = [⟨0⟩, ⟨6⟩, ⟨12⟩]
