class virtual ['fluent, 'action] t =
object
  method virtual to_string : string
  method virtual to_ipc_string : string
  method virtual valid : 'fluent array -> 'fluent array -> (bool * int)
end
