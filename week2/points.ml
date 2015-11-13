(*  prelude
type point  = { x : float; y : float; z : float }
type dpoint = { dx : float; dy : float; dz : float }
type physical_object = { position : point; velocity : dpoint }
*)

let move p dp =
  {x = p.x +. dp.dx; y = p.y +. dp.dy; z = p.z +. dp.dz};;

let next obj =
  {position = move obj.position obj.velocity; velocity = obj.velocity};;

let will_collide_soon p1 p2 =
  let p1n = next p1 and p2n = next p2 in
  let dx = p1n.position.x -. p2n.position.x and
    dy = p1n.position.y -. p2n.position.y and
    dz = p1n.position.z -. p2n.position.z in
  let x2 = dx *. dx and y2 = dy *. dy and z2 = dz *. dz in
  let d2 = x2 +. y2 +. z2 in
  d2 <= 4.0;;
