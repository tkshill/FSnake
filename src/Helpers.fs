namespace Helpers


module Helpers =
  open System

  /// Reverse the order of the parameters a binary function. So f a b becomes f b a
  let flip f = fun a b -> f b a

  /// Wraps a value in a unary function. So a becomes fun _ -> a
  let callback value = fun _ -> value

  let shuffle<'a> : 'a seq -> 'a seq = Seq.sortBy (fun _ -> (new Random()).Next())
