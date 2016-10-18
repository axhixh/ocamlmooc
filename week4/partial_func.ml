let ccr = fun a -> 
  let a' = 8. *. cos (a /. 2.) in 
  fun b -> 
    let ab' = a' *. cos (b /. 2.) in
    fun c -> 
      let abc' = ab' *. cos (c /. 2.) in
      fun s -> s /. abc';;