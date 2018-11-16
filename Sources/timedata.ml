class t timeset_struct (code : int) =
object (self : 'timedata)

  val mutable timeset_struct = timeset_struct
  val mutable timeset = (-1.0,-1.0)
  val mutable closed_left = true
  val mutable closed_right = true
  val mutable interval_type = 0
  val mutable code = code
  val mutable min_dur = 0.0

  method timeset = timeset
  method closed_left = closed_left
  method closed_right = closed_right
  method set_closed_left b = closed_left <- b
  method set_closed_right b = closed_right <- b
  method code = code
  method set_timeset (ts : float * float) = timeset <- ts
  method set_code c = code <- c
  method set_min_dur d = min_dur <- d

  method to_string =
    (if closed_left then "[" else "]") ^ (string_of_float (fst self#timeset)) ^ ";" ^ (string_of_float (snd self#timeset)) ^ (if closed_right then "]" else "[")

  method to_complete_string =
    let is_timepoint = ((fst timeset) == (snd timeset)) in
    (if code == 0 then if is_timepoint then "{AT " else "{OVER "
    else if code == 1 then "{SOMEWHERE "
    else if code == 2 then if min_dur == 0.0 then "{ANYWHERE "
         else "{MIN_DUR " ^ string_of_float min_dur ^ " ANYWHERE "
    else if code == 10 then "{-> OVER "
    else "{") ^ (if is_timepoint then (string_of_float (fst timeset)) else self#to_string) ^ "}"

end