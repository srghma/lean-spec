import Pipes
import Spec.Event

abbrev TestEvents := Producer Spec.Event IO (Array (Spec.Tree String PEmpty Spec.Result))

abbrev Reporter := Pipe Spec.Event Spec.Event IO (Array (Spec.Tree String PEmpty Spec.Result))
