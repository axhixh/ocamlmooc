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
      let {position = {x=x1; y=y1; z=z1}} = next p1 and
      {position = {x=x2; y=y2; z=z2}} = next p2 in
      let dx = x2 -. x1 and
          dy = y2 -. y1 and
          dz = z2 -. z1 in
      dx *. dx +. dy *. dy +. dz *. dz <= 4.;;

