(* Copyright (c) 2007 Scott Lembcke
 * (Converted to OCaml by F. Monnier)
 * 
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 * 
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 * 
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 *)
 
#directory "+glMLite"
#load "GL.cma"
#load "Glu.cma"
#load "Glut.cma"

open GL
open Glu
open Glut

#directory "+chipmunk"
#load "chipmunk.cma"
open Chipmunk
open Low_level
open OO

let sleep_ticks = 16
let draw_bounding_box = false

let cpv = vec2d ;;
let cpvi(x,y) = {cp_x=float x; cp_y= float y} ;;

let shapes_li = ref [] ;;
let bodies_li = ref [] ;;

let _space = ref None
let _staticBody = ref None

let get_global g = match !g with Some v -> v | None -> raise Exit ;;

let cpvzero = cpvzero() ;;
let pi = 4.0 *. atan 1.0 ;;

(* {{{ Demo 1 *)

let demo1_update ~ticks =
  let space = get_global _space in

  let steps = 2 in
  let dt = 1.0 /. 60.0 /. (float steps) in
  
  for i=0 to pred steps do
    cpSpaceStep ~space ~dt;
  done;
;;


let demo1_init() =
  let staticBody = cpBodyNew infinity infinity in
  _staticBody := Some staticBody;

  cpResetShapeIdCounter();
  let space = cpSpaceNew() in
  cpSpaceResizeStaticHash space 20.0 999;
  cpSpaceSetGravity space (cpvi(0, -100));
  _space := Some space;
  
  let verts = [|
    cpv(-15.,-15.);
    cpv(-15., 15.);
    cpv( 15., 15.);
    cpv( 15.,-15.);
  |] in
  
  let shape = cpSegmentShapeNew staticBody (cpvi(-320,-240)) (cpvi(-320,240)) 0.0 in
  cpShapeSetElasticity shape 1.0;
  cpShapeSetFriction shape 1.0;
  cpSpaceAddStaticShape space shape;
  shapes_li := shape :: !shapes_li;

  let shape = cpSegmentShapeNew staticBody (cpvi(320,-240)) (cpvi(320,240)) 0.0 in
  cpShapeSetElasticity shape 1.0;
  cpShapeSetFriction shape 1.0;
  cpSpaceAddStaticShape space shape;
  shapes_li := shape :: !shapes_li;

  let shape = cpSegmentShapeNew staticBody (cpvi(-320,-240)) (cpvi(320,-240)) 0.0 in
  cpShapeSetElasticity shape 1.0;
  cpShapeSetFriction shape 1.0;
  cpSpaceAddStaticShape space shape;
  shapes_li := shape :: !shapes_li;

    
  for i=0 to pred 50 do
    let j = i + 1 in
    let a = cpvi(i * 10 - 320, i * -10 + 240)
    and b = cpvi(j * 10 - 320, i * -10 + 240)
    and c = cpvi(j * 10 - 320, j * -10 + 240) in
    
    let shape = cpSegmentShapeNew staticBody a b 0.0 in
    cpShapeSetElasticity shape 1.0;
    cpShapeSetFriction shape 1.0;
    cpSpaceAddStaticShape space shape;
    shapes_li := shape :: !shapes_li;
    
    let shape = cpSegmentShapeNew staticBody b c 0.0 in
    cpShapeSetElasticity shape 1.0;
    cpShapeSetFriction shape 1.0;
    cpSpaceAddStaticShape space shape;
    shapes_li := shape :: !shapes_li;
  done;

  let body = cpBodyNew 1.0 (cpMomentForPoly 1.0 verts cpvzero) in
  cpBodySetPos body (cpvi(-280, 240));
  cpSpaceAddBody space body;
  let shape = cpPolyShapeNew body verts cpvzero in
  cpShapeSetElasticity shape 0.0;
  cpShapeSetFriction shape 1.5;
  cpShapeSetCollisionType shape 1;
  cpSpaceAddShape space shape;
  bodies_li := body :: !bodies_li;
  shapes_li := shape :: !shapes_li;
;;

(* }}} end of demo 1 *)
(* {{{ Demo 2 *)

let demo2_update ~ticks =
  let space = get_global _space in
  
  let steps = 1 in
  let dt = 1.0 /. 60.0 /. (float steps) in
  
  for i=0 to pred steps do
    cpSpaceStep space dt;
  done;
;;

let demo2_init() =
  let staticBody = cpBodyNew infinity infinity in
  _staticBody := Some staticBody;

  cpResetShapeIdCounter();
  let  space = cpSpaceNew() in
  _space := Some space;

  cpSpaceSetIterations space 20;
  cpSpaceResizeStaticHash space 40.0 1000;
  cpSpaceResizeActiveHash space 40.0 1000;
  cpSpaceSetGravity space (cpv(0., -100.));
  
  
  let verts = [|
    cpv(-15.,-15.);
    cpv(-15., 15.);
    cpv( 15., 15.);
    cpv( 15.,-15.);
  |] in
  
  let make_seg (pt1, pt2) =
    let shape = cpSegmentShapeNew staticBody pt1 pt2 0.0 in
    cpShapeSetElasticity shape 1.0;
    cpShapeSetFriction shape 1.0;
    cpSpaceAddStaticShape space shape;
    shapes_li := shape :: !shapes_li;
  in
  List.iter make_seg [
      cpv(-320.,-240.), cpv(-320., 240.);
      cpv( 320.,-240.), cpv( 320., 240.);
      cpv(-320.,-240.), cpv( 320.,-240.);
    ];

  
  for i=0 to pred 14 do
    for j=0 to i do
      let body = cpBodyNew 1.0 (cpMomentForPoly 1.0 verts cpvzero) in
      cpBodySetPos body (cpv (float(j*32 - i*16), float(300 - i*32)));
      cpSpaceAddBody space body;
      let shape = cpPolyShapeNew body verts cpvzero in
      cpShapeSetElasticity shape 0.0;
      cpShapeSetFriction shape 0.8;
      cpSpaceAddShape space shape;
      bodies_li := body :: !bodies_li;
      shapes_li := shape :: !shapes_li;
    done;
  done;
  
  
  let radius = 15.0 in
  let body = cpBodyNew 10.0 (cpMomentForCircle 10.0 0.0 radius cpvzero) in
  cpBodySetPos body (cpv(0., radius -. 240.));
(*  cpBodySetVel (cpv(10., 0.)); *)
  cpSpaceAddBody space body;
  let shape = cpCircleShapeNew body radius cpvzero in
  cpShapeSetElasticity shape 0.0;
  cpShapeSetFriction shape 0.9;
  cpSpaceAddShape space shape;
  bodies_li := body :: !bodies_li;
  shapes_li := shape :: !shapes_li;
;;

(* }}} end of demo 2 *)
(* {{{ Demo 3 *)

let rand_max = 100_000 ;;
let rand() = Random.float (float rand_max) ;;

let eachBody body =
  let p = cpBodyGetPos body in
	if (p.cp_y < -260. || abs_float p.cp_x > 340.) then
  begin
    let x = rand() /. (float rand_max) *. 640. -. 320. in
		cpBodySetPos body (cpv(x, 260.));
  end;
;;


let demo3_update ~ticks =
  let steps = 1 in
  let dt = 1.0 /. 60.0 /. (float steps) in

  let space = get_global _space in

	for i=0 to pred steps do
		cpSpaceStep ~space ~dt;
    (*
		cpSpaceEachBody(space, &eachBody, NULL);
    *)
    List.iter eachBody !bodies_li;
  done;
;;

let num_verts = 5

let demo3_init() =
  let staticBody = cpBodyNew infinity infinity in
  _staticBody := Some staticBody;
	
	cpResetShapeIdCounter();
  let space = cpSpaceNew() in
	cpSpaceSetIterations space 5;
	cpSpaceSetGravity space (cpvi(0, -100));
  _space := Some space;
	
	cpSpaceResizeStaticHash space 40.0 999;
	cpSpaceResizeActiveHash space 30.0 2999;
	
  let rec make_verts acc i =
    if i >= num_verts then Array.of_list(List.rev acc)
    else begin
      let angle = (-2.) *. pi *. (float i) /. (float num_verts) in
      let vert = cpv(10. *. (cos angle), 10. *. (sin angle)) in
      make_verts (vert::acc) (succ i)
    end
  in
  let verts = make_verts [] 0 in
	
  let tris = [|
		cpvi(-15,-15);
		cpvi(  0, 10);
		cpvi( 15,-15);
  |] in

	for i=0 to pred 9 do
		for j=0 to pred 6 do
      let stagger = (j mod 2) * 40 in
      let offset = cpvi(i*80 - 320 + stagger, j*70 - 240) in
      let shape = cpPolyShapeNew staticBody tris offset in
      cpShapeSetElasticity shape 1.0;
      cpShapeSetFriction shape 1.0;
			cpSpaceAddStaticShape space shape;
      shapes_li := shape :: !shapes_li;
    done;
  done;
		
	for i=0 to pred 300 do
    let body = cpBodyNew 1.0 (cpMomentForPoly 1.0 verts cpvzero) in
    (*
    let body = cpBodyNew 1.0 (cpMomentForCircle 1.0 0.0 9.0 cpvzero) in
    *)
    let x = rand() /. (float rand_max) *. 640. -. 320. in
		cpBodySetPos body (cpv(x, 350.));
		cpSpaceAddBody space body;
    let shape = cpPolyShapeNew body verts cpvzero in
    (*
    let shape = cpCircleShapeNew body 9.0 cpvzero in
    *)
    cpShapeSetElasticity shape 0.0;
    cpShapeSetFriction shape 0.4;
		cpSpaceAddShape space shape;
    shapes_li := shape :: !shapes_li;
    bodies_li := body :: !bodies_li;
  done;
;;

(* }}} *)
(* {{{ Demo 4 *)

let demo4_update ~ticks =
  let steps = 3 in
  let dt = 1.0 /. 60.0 /. (float steps) in

  let space = get_global _space
  and staticBody = get_global _staticBody in
  
  for i=0 to pred steps do
    cpSpaceStep ~space ~dt;
    cpBodyUpdatePosition staticBody dt;
    cpSpaceRehashStatic ~space;
  done
;;

let demo4_init() =
  let staticBody = cpBodyNew infinity infinity in
  _staticBody := Some staticBody;
  
  cpResetShapeIdCounter();
  let space = cpSpaceNew() in
  _space := Some space;
  cpSpaceResizeActiveHash space  30.0  999;
  cpSpaceResizeStaticHash space  200.0  99;

  cpSpaceSetGravity ~space ~gravity:(cpv(0., -600.));
  
  let verts = [|
    cpv(-30., -15.);
    cpv(-30.,  15.);
    cpv( 30.,  15.);
    cpv( 30., -15.); |]
  in
  
  let a = cpv(-200., -200.)
  and b = cpv(-200.,  200.)
  and c = cpv( 200.,  200.)
  and d = cpv( 200., -200.) in
  
  let shape = cpSegmentShapeNew staticBody a b 0.0 in
  cpShapeSetElasticity shape 1.0;
  cpShapeSetFriction shape 1.0;
  shapes_li := shape :: !shapes_li;

  let new_segment pt1 pt2 =
    let shape = cpSegmentShapeNew staticBody pt1 pt2 0.0 in
    cpShapeSetElasticity shape 1.0;
    cpShapeSetFriction shape 1.0;
    cpSpaceAddStaticShape ~space ~shape;

    shapes_li := shape :: !shapes_li;
  in
  new_segment a b;
  new_segment b c;
  new_segment c d;
  new_segment d a;

  cpBodySetAVel staticBody 0.4;
  
  for i=0 to pred 3 do
    for j=0 to pred 7 do
      let body = cpBodyNew 1.0 (cpMomentForPoly 1.0 verts cpvzero) in
      cpBodySetPos ~body ~p:(cpv (float(i*60 - 150),
                float(j*30 - 150)) );
      cpSpaceAddBody space body;
      let shape = cpPolyShapeNew body verts cpvzero in
      cpShapeSetElasticity shape 0.0;
      cpShapeSetFriction shape 0.7;
      cpSpaceAddShape space shape;

      bodies_li := body :: !bodies_li;
      shapes_li := shape :: !shapes_li;
    done;
  done;
;;

(* }}} end of demo 4 *)
(* {{{ Demo 5 *)

let demo5_update ~ticks =
  let steps = 2 in
  let dt = 1.0 /. 60.0 /. (float steps) in
  
  let space = get_global _space in

  for i=0 to pred steps do
    cpSpaceStep ~space ~dt;
  done;
;;

let demo5_init() =
  let staticBody = cpBodyNew infinity infinity in
  _staticBody := Some staticBody;
  
  cpResetShapeIdCounter();
  let space = cpSpaceNew() in
  cpSpaceSetIterations space 20;
  cpSpaceResizeActiveHash space 40.0 2999;
  cpSpaceResizeStaticHash space 40.0 999;
  cpSpaceSetGravity space (cpvi(0, -300));
  _space := Some space;
  
  let verts = [|
    cpvi(-3,-20);
    cpvi(-3, 20);
    cpvi( 3, 20);
    cpvi( 3,-20);
  |] in
  
  let shape = cpSegmentShapeNew staticBody (cpvi(-600,-240)) (cpvi(600,-240)) 0.0 in
  cpShapeSetElasticity shape 1.0;
  cpShapeSetFriction shape 1.0;
  cpSpaceAddStaticShape space shape;
  shapes_li := shape :: !shapes_li;

  let u = 0.6 in
  
  let n = 9 in
  for i=1 to n do
    let offset = cpv((-.(float i) *. 60. /. 2.0), float((n - i)*52)) in
    
    for j=0 to pred i do
      let body = cpBodyNew 1.0 (cpMomentForPoly 1.0 verts cpvzero) in
      cpBodySetPos ~body ~p:(cpvadd (cpvi(j*60, -220)) offset);
      cpSpaceAddBody space body;
      let shape = cpPolyShapeNew body verts cpvzero in
      cpShapeSetElasticity shape 0.0;
      cpShapeSetFriction shape u;
      cpSpaceAddShape space shape;
      shapes_li := shape :: !shapes_li;
      bodies_li := body :: !bodies_li;

      let body = cpBodyNew 1.0 (cpMomentForPoly 1.0 verts cpvzero) in
      cpBodySetPos body (cpvadd(cpvi(j*60, -197)) offset);
      cpBodySetAngle body (pi /. 2.0);
      cpSpaceAddBody space body;
      let shape = cpPolyShapeNew body verts cpvzero in
      cpShapeSetElasticity shape 0.0;
      cpShapeSetFriction shape u;
      cpSpaceAddShape space shape;
      shapes_li := shape :: !shapes_li;
      bodies_li := body :: !bodies_li;
      
      if(j <> (i - 1)) then begin
        let body = cpBodyNew 1.0 (cpMomentForPoly 1.0 verts cpvzero) in
        cpBodySetPos body (cpvadd(cpvi(j*60 + 30, -191)) offset);
        cpBodySetAngle body (pi /. 2.0);
        cpSpaceAddBody space body;
        let shape = cpPolyShapeNew body verts cpvzero in
        cpShapeSetElasticity shape 0.0;
        cpShapeSetFriction shape u;
        cpSpaceAddShape space shape;
        shapes_li := shape :: !shapes_li;
        bodies_li := body :: !bodies_li;
      end;
    done;

    let body = cpBodyNew 1.0 (cpMomentForPoly 1.0 verts cpvzero) in
    cpBodySetPos body (cpvadd(cpvi(-17, -174)) offset);
    cpSpaceAddBody space body;
    let shape = cpPolyShapeNew body verts cpvzero in
    cpShapeSetElasticity shape 0.0;
    cpShapeSetFriction shape u;
    cpSpaceAddShape space shape;
    shapes_li := shape :: !shapes_li;
    bodies_li := body :: !bodies_li;

    let body = cpBodyNew 1.0 (cpMomentForPoly 1.0 verts cpvzero) in
    cpBodySetPos body (cpvadd(cpvi((i - 1)*60 + 17, -174)) offset);
    cpSpaceAddBody space body;
    let shape = cpPolyShapeNew body verts cpvzero in
    cpShapeSetElasticity shape 0.0;
    cpShapeSetFriction shape u;
    cpSpaceAddShape space shape;
    shapes_li := shape :: !shapes_li;
    bodies_li := body :: !bodies_li;

    if i = n then begin
      cpBodySetAVel body (-1.);
      let w = cpBodyGetAVel body in
      cpBodySetVel body (cpv( (-.w) *. 20., 0.));
    end;
  done;
;;

(* }}} end of demo 5 *)
(* {{{ Demo 6 *)

let demo6_update ~ticks =
  let steps = 2 in
  let dt = 1.0 /. 60.0 /. (float steps) in

  let space = get_global _space in

  for i=0 to pred steps do
    cpSpaceStep ~space ~dt;
  done;
;;


let demo6_init() =
  let staticBody = cpBodyNew infinity infinity in
  _staticBody := Some staticBody;
  
  cpResetShapeIdCounter();
  let space = cpSpaceNew() in
  cpSpaceResizeStaticHash space 20.0 999;
  cpSpaceSetGravity space (cpvi(0, -100));
  _space := Some space;
  
  
  let verts = [|
    cpvi(-7,-15);
    cpvi(-7, 15);
    cpvi( 7, 15);
    cpvi( 7,-15);
  |] in
  
  let shape = cpSegmentShapeNew staticBody (cpvi(-320,-240)) (cpvi(-320,240)) 0.0 in
  cpShapeSetElasticity shape 1.0;
  cpShapeSetFriction shape 1.0;
  cpSpaceAddStaticShape space shape;
  shapes_li := shape :: !shapes_li;

  let shape = cpSegmentShapeNew staticBody (cpvi(320,-240)) (cpvi(320,240)) 0.0 in
  cpShapeSetElasticity shape 1.0;
  cpShapeSetFriction shape 1.0;
  cpSpaceAddStaticShape space shape;
  shapes_li := shape :: !shapes_li;

  let shape = cpSegmentShapeNew staticBody (cpvi(-320,-240)) (cpvi(320,-240)) 0.0 in
  cpShapeSetElasticity shape 1.0;
  cpShapeSetFriction shape 1.0;
  cpSpaceAddStaticShape space shape;
  shapes_li := shape :: !shapes_li;

  for i=0 to pred 50 do
    let j = i + 1 in
    let a = cpvi(i*10 - 320, i* -10 + 240)
    and b = cpvi(j*10 - 320, i* -10 + 240)
    and c = cpvi(j*10 - 320, j* -10 + 240) in
    
    let shape = cpSegmentShapeNew staticBody a b 0.0 in
    cpShapeSetElasticity shape 1.0;
    cpShapeSetFriction shape 1.0;
    cpSpaceAddStaticShape space shape;
    shapes_li := shape :: !shapes_li;
    
    let shape = cpSegmentShapeNew staticBody b c 0.0 in
    cpShapeSetElasticity shape 1.0;
    cpShapeSetFriction shape 1.0;
    cpSpaceAddStaticShape space shape;
    shapes_li := shape :: !shapes_li;
  done;

  let moment = cpMomentForPoly 1.0 verts (cpvi(0,-15)) in
  let moment = moment +. (cpMomentForCircle 1.0 0.0 25.0 (cpvi(0,15))) in
  let body = cpBodyNew 1.0  moment in
  cpBodySetPos body (cpvi(-280, 250));
  cpBodySetAVel body 1.0;
  cpSpaceAddBody space body;
  bodies_li := body :: !bodies_li;

  let shape = cpPolyShapeNew body verts (cpvi(0,-15)) in
  cpShapeSetElasticity shape 0.0;
  cpShapeSetFriction shape 1.5;
  cpSpaceAddShape space shape;
  shapes_li := shape :: !shapes_li;

  let shape = cpCircleShapeNew body 25.0 (cpvi(0,15)) in
  cpShapeSetElasticity shape 0.9;
  cpShapeSetFriction shape 1.5;
  cpSpaceAddShape space shape;
  shapes_li := shape :: !shapes_li;
;;

(* }}} end of demo 6 *)
(* {{{ Demo 7 *)

let _chassis = ref None
let _wheel1 = ref None
let _wheel2 = ref None

let demo7_update ~ticks =
  let steps = 3 in
  let dt = 1.0 /. 60.0 /. (float steps) in
  
  let space = get_global _space
  and chassis = get_global _chassis
  and wheel1 = get_global _wheel1
  and wheel2 = get_global _wheel2 in

  for i=0 to pred steps do
    cpBodyResetForces chassis;
    cpBodyResetForces wheel1;
    cpBodyResetForces wheel2;
    cpDampedSpring chassis wheel1 (cpvi(40, 15)) cpvzero 50.0 150.0 10.0 dt;
    cpDampedSpring chassis wheel2 (cpvi(-40, 15)) cpvzero 50.0 150.0 10.0 dt;
    
    cpSpaceStep ~space ~dt;
  done;
;;


let _make_box space ~x ~y =
  let verts = [|
    cpvi(-15,-7);
    cpvi(-15, 7);
    cpvi( 15, 7);
    cpvi( 15,-7);
  |] in
  
  let body = cpBodyNew 1.0 (cpMomentForPoly 1.0 verts cpvzero) in
  cpBodySetPos body (cpv(x, y));
  cpSpaceAddBody space body;
  let shape = cpPolyShapeNew body verts cpvzero in
  cpShapeSetElasticity shape 0.0;
  cpShapeSetFriction shape 1.0;
  cpSpaceAddShape space shape;
  shapes_li := shape :: !shapes_li;
  bodies_li := body :: !bodies_li;

  (body)
;;


let demo7_init() =
  let staticBody = cpBodyNew infinity infinity in
  _staticBody := Some staticBody;

  cpResetShapeIdCounter();
  let space = cpSpaceNew() in
  cpSpaceSetIterations space 10;
  cpSpaceResizeActiveHash space 50.0 999;
  cpSpaceResizeStaticHash space 50.0 999;
  cpSpaceSetGravity space (cpvi(0, -300));
  _space := Some space;

  let shape = cpSegmentShapeNew staticBody (cpvi(-320,-240)) (cpvi(-320,240)) 0.0 in
  cpShapeSetElasticity shape 1.0;
  cpShapeSetFriction shape 1.0;
  cpSpaceAddStaticShape space shape;
  shapes_li := shape :: !shapes_li;
  
  let shape = cpSegmentShapeNew staticBody (cpvi(320,-240)) (cpvi(320,240)) 0.0 in
  cpShapeSetElasticity shape 1.0;
  cpShapeSetFriction shape 1.0;
  cpSpaceAddStaticShape space shape;
  shapes_li := shape :: !shapes_li;
  
  let shape = cpSegmentShapeNew staticBody (cpvi(-320,-240)) (cpvi(320,-240)) 0.0 in
  cpShapeSetElasticity shape 1.0;
  cpShapeSetFriction shape 1.0;
  cpSpaceAddStaticShape space shape;
  shapes_li := shape :: !shapes_li;
  
  let shape = cpSegmentShapeNew staticBody (cpvi(-320,70)) (cpvi(0,-240)) 0.0 in
  cpShapeSetElasticity shape 1.0;
  cpShapeSetFriction shape 1.0;
  cpSpaceAddStaticShape space shape;
  shapes_li := shape :: !shapes_li;
  
  let shape = cpSegmentShapeNew staticBody (cpvi(0,-240)) (cpvi(320,-200)) 0.0 in
  cpShapeSetElasticity shape 1.0;
  cpShapeSetFriction shape 1.0;
  cpSpaceAddStaticShape space shape;
  shapes_li := shape :: !shapes_li;
  
  let shape = cpSegmentShapeNew staticBody (cpvi(200,-240)) (cpvi(320,-100)) 0.0 in
  cpShapeSetElasticity shape 1.0;
  cpShapeSetFriction shape 1.0;
  cpSpaceAddStaticShape space shape;
  shapes_li := shape :: !shapes_li;
  

  let make_box = _make_box space in

  let body1 = make_box (-100.) (100.); in
  let body2 = make_box ((cpBodyGetPos body1).cp_x +. 40.)  100. in
  let body3 = make_box ((cpBodyGetPos body2).cp_x +. 40.)  100. in
  let body4 = make_box ((cpBodyGetPos body3).cp_x +. 40.)  100. in
  let body5 = make_box ((cpBodyGetPos body4).cp_x +. 40.)  100. in
  let body6 = make_box ((cpBodyGetPos body5).cp_x +. 40.)  100. in
  let body7 = make_box ((cpBodyGetPos body6).cp_x +. 40.)  100. in
  
  let joint = cpPivotJointNew staticBody body1 (cpv((cpBodyGetPos body1).cp_x -. 20., 100.)) in
  cpSpaceAddJoint space joint;
  
  let joint = cpPivotJointNew body1 body2 (cpv((cpBodyGetPos body2).cp_x -. 20., 100.)) in
  cpSpaceAddJoint space  joint;
  
  let joint = cpPivotJointNew body2 body3 (cpv((cpBodyGetPos body3).cp_x -. 20., 100.)) in
  cpSpaceAddJoint space joint;
  
  let joint = cpPivotJointNew body3 body4 (cpv((cpBodyGetPos body4).cp_x -. 20., 100.)) in
  cpSpaceAddJoint space joint;
  
  let joint = cpPivotJointNew body4 body5 (cpv((cpBodyGetPos body5).cp_x -. 20., 100.)) in
  cpSpaceAddJoint space joint;
  
  let joint = cpPivotJointNew body5 body6 (cpv((cpBodyGetPos body6).cp_x -. 20., 100.)) in
  cpSpaceAddJoint space joint;
  
  let joint = cpPivotJointNew body6 body7 (cpv((cpBodyGetPos body7).cp_x -. 20., 100.)) in
  cpSpaceAddJoint space joint;
  
  let joint = cpPivotJointNew body7 staticBody (cpv((cpBodyGetPos body7).cp_x +. 20., 100.)) in
  cpSpaceAddJoint space joint;
  
  
  let body1 = make_box (-100.) (50.) in
  let body2 = make_box ((cpBodyGetPos body1).cp_x +. 40.) 50. in
  let body3 = make_box ((cpBodyGetPos body2).cp_x +. 40.) 50. in
  let body4 = make_box ((cpBodyGetPos body3).cp_x +. 40.) 50. in
  let body5 = make_box ((cpBodyGetPos body4).cp_x +. 40.) 50. in
  let body6 = make_box ((cpBodyGetPos body5).cp_x +. 40.) 50. in
  let body7 = make_box ((cpBodyGetPos body6).cp_x +. 40.) 50. in
  
  let max = 25.0
  and min = 10.0 in
  
  let joint = cpSlideJointNew staticBody body1 (cpv((cpBodyGetPos body1).cp_x -. 15. -. 10., 50.))
                                               (cpvi(-15, 0)) min max in
  cpSpaceAddJoint space joint;
  
  let joint = cpSlideJointNew body1 body2 (cpvi(15, 0)) (cpvi(-15, 0)) min max in
  cpSpaceAddJoint space joint;
  
  let joint = cpSlideJointNew body2 body3 (cpvi(15, 0)) (cpvi(-15, 0)) min max in
  cpSpaceAddJoint space joint;
  
  let joint = cpSlideJointNew body3 body4 (cpvi(15, 0)) (cpvi(-15, 0)) min max in
  cpSpaceAddJoint space joint;
  
  let joint = cpSlideJointNew body4 body5 (cpvi(15, 0)) (cpvi(-15, 0)) min max in
  cpSpaceAddJoint space joint;
  
  let joint = cpSlideJointNew body5 body6 (cpvi(15, 0)) (cpvi(-15, 0)) min max in
  cpSpaceAddJoint space joint;
  
  let joint = cpSlideJointNew body6 body7 (cpvi(15, 0)) (cpvi(-15, 0)) min max in
  cpSpaceAddJoint space joint;
  
  let joint = cpSlideJointNew body7 staticBody (cpvi(15, 0))
                                               (cpv((cpBodyGetPos body7).cp_x +. 15. +. 10., 50.)) min max in
  cpSpaceAddJoint space joint;
  
  let body1 = make_box (-100.) 150. in
  let body2 = make_box ((cpBodyGetPos body1).cp_x +. 40.) 150. in
  let body3 = make_box ((cpBodyGetPos body2).cp_x +. 40.) 150. in
  let body4 = make_box ((cpBodyGetPos body3).cp_x +. 40.) 150. in
  let body5 = make_box ((cpBodyGetPos body4).cp_x +. 40.) 150. in
  let body6 = make_box ((cpBodyGetPos body5).cp_x +. 40.) 150. in
  let body7 = make_box ((cpBodyGetPos body6).cp_x +. 40.) 150. in
  
  let joint = cpPinJointNew staticBody body1 (cpv((cpBodyGetPos body1).cp_x -. 15. -. 10., 150.))
                                             (cpvi(-15, 0)) in
  cpSpaceAddJoint space joint;
  
  let joint = cpPinJointNew body1 body2 (cpvi(15, 0)) (cpvi(-15, 0)) in
  cpSpaceAddJoint space joint;
  
  let joint = cpPinJointNew body2 body3 (cpvi(15, 0)) (cpvi(-15, 0)) in
  cpSpaceAddJoint space joint;
  
  let joint = cpPinJointNew body3 body4 (cpvi(15, 0)) (cpvi(-15, 0)) in
  cpSpaceAddJoint space joint;
  
  let joint = cpPinJointNew body4 body5 (cpvi(15, 0)) (cpvi(-15, 0)) in
  cpSpaceAddJoint space joint;
  
  let joint = cpPinJointNew body5 body6 (cpvi(15, 0)) (cpvi(-15, 0)) in
  cpSpaceAddJoint space joint;
  
  let joint = cpPinJointNew body6 body7 (cpvi(15, 0)) (cpvi(-15, 0)) in
  cpSpaceAddJoint space joint;
  
  let joint = cpPinJointNew body7 staticBody (cpvi(15, 0)) (cpv((cpBodyGetPos body7).cp_x +. 15. +. 10., 150.)) in
  cpSpaceAddJoint space joint;
  
  let body1 = make_box 190. 200. in
  let joint = cpGrooveJointNew staticBody body1 (cpvi(0, 195)) (cpvi(250, 200)) (cpvi(-15, 0)) in
  cpSpaceAddJoint space joint;
  
  let verts = [|
    cpvi(-20,-15);
    cpvi(-20, 15);
    cpvi( 20, 15);
    cpvi( 20,-15);
  |] in
  
  let chassis = cpBodyNew 10.0 (cpMomentForPoly 10.0 verts (cpvi(0,0))) in
  cpBodySetPos chassis (cpvi(-200, 100));
(* cpBodySetVel body (cpvi(200, 0)); *)
  cpSpaceAddBody space chassis;
  let shape = cpPolyShapeNew chassis verts (cpvi(0,0)) in
  cpShapeSetElasticity shape 0.0;
  cpShapeSetFriction shape 1.0;
  cpSpaceAddShape space shape;
  _chassis := Some chassis;
  shapes_li := shape :: !shapes_li;
  bodies_li := chassis :: !bodies_li;
  
  let radius = 15 in
  let wheel_mass = 0.3 in
  let offset = cpvi(radius + 30, -25) in
  let wheel1 = cpBodyNew wheel_mass (cpMomentForCircle wheel_mass 0.0 (float radius) cpvzero) in
  cpBodySetPos wheel1 (cpvadd (cpBodyGetPos chassis) offset);
  cpBodySetVel wheel1 (cpBodyGetVel chassis);
  cpSpaceAddBody space wheel1;
  let shape = cpCircleShapeNew wheel1 (float radius) cpvzero in
  cpShapeSetElasticity shape 0.0;
  cpShapeSetFriction shape 2.5;
  cpSpaceAddShape space shape;
  _wheel1 := Some wheel1;
  shapes_li := shape :: !shapes_li;
  
  let joint = cpPinJointNew chassis wheel1 cpvzero cpvzero in
  cpSpaceAddJoint space joint;
  
  
  let wheel2 = cpBodyNew wheel_mass (cpMomentForCircle wheel_mass 0.0 (float radius) cpvzero) in
  cpBodySetPos wheel2 (cpvadd (cpBodyGetPos chassis) (cpv(-.offset.cp_x, offset.cp_y)));
  cpBodySetVel wheel2 (cpBodyGetVel chassis);
  cpSpaceAddBody space wheel2;
  let shape = cpCircleShapeNew wheel2 (float radius) cpvzero in
  cpShapeSetElasticity shape 0.0;
  cpShapeSetFriction shape 2.5;
  cpSpaceAddShape space shape;
  _wheel2 := Some wheel2;
  shapes_li := shape :: !shapes_li;

  let joint = cpPinJointNew chassis wheel2 cpvzero cpvzero in
  cpSpaceAddJoint space joint;
;;

(* }}} end of demo 7 *)

let demo_destroy() =
  bodies_li := [];
  shapes_li := [];

  let space = get_global _space
  and staticBody = get_global _staticBody in
  
  cpSpaceFreeChildren ~space;
  cpSpaceFree ~space;
  
  cpBodyFree staticBody;
;;


let init_funcs = [|
    demo1_init;
    demo2_init;
    demo3_init;
    demo4_init;
    demo5_init;
    demo6_init;
    demo7_init;
  |]

let update_funcs = [|
    demo1_update;
    demo2_update;
    demo3_update;
    demo4_update;
    demo5_update;
    demo6_update;
    demo7_update;
  |]

let destroy_funcs = [|
    demo_destroy;
    demo_destroy;
    demo_destroy;
    demo_destroy;
    demo_destroy;
    demo_destroy;
    demo_destroy;
  |]

let demo_index = ref 0 ;;

let ticks = ref 0 ;;

(* {{{ Draw Stuff *)

let drawCircle ~center:{cp_x=x; cp_y=y} ~r ~a =
  let segs = 15 in
  let coef = 2.0 *. pi /. (float segs) in

  glColor3 0.0 0.0 0.0;
  glBegin GL_LINE_STRIP;
    for n=0 to segs do
      let rads = (float n) *. coef in
      glVertex2 (r *. cos(rads +. a) +. x)
                (r *. sin(rads +. a) +. y);
    done;
    glVertex2 x y;
  glEnd();
;;


let drawCircleShape ~shape =
  let body = cpShapeGetBody ~shape in
  let circle = cpCircleShape_of_cpShape ~shape in
  let center = cpvadd (cpBodyGetPos ~body)
                      (cpvrotate (cpCircleShapeGetCenter circle)
                                 (cpBodyGetRot ~body))
  in
  drawCircle center (cpCircleShapeGetRadius circle) (cpBodyGetAngle ~body);
;;


let drawSegmentShape ~shape =
  let body = cpShapeGetBody ~shape
  and seg = cpSegmentShape_of_cpShape ~shape in
  let seg_a = cpSegmentShapeGetA seg
  and seg_b = cpSegmentShapeGetB seg in
  let a = cpvadd (cpBodyGetPos ~body) (cpvrotate (seg_a) (cpBodyGetRot ~body))
  and b = cpvadd (cpBodyGetPos ~body) (cpvrotate (seg_b) (cpBodyGetRot ~body)) in

  glColor3 0.0 0.0 0.0;
  glBegin GL_LINES;
    glVertex2  a.cp_x  a.cp_y;
    glVertex2  b.cp_x  b.cp_y;
  glEnd();
;;


let drawPolyShape ~shape =
  let body = cpShapeGetBody ~shape
  and poly = cpPolyShape_of_cpShape ~shape in
  
  let num = cpPolyShapeGetNumVerts ~poly
  and verts = cpPolyShapeGetVects ~poly in

  glColor3 0.0 0.0 0.0;
  glBegin GL_LINE_LOOP;
  for i=0 to pred num do
    let v = cpvadd (cpBodyGetPos ~body)
             (cpvrotate verts.(i) (cpBodyGetRot ~body)) in
    glVertex2 v.cp_x v.cp_y;
  done;
  glEnd();
;;


let drawBB ~body ~bb =
  let l = Chipmunk.Low_level.cpBBGetL bb
  and b = cpBBGetB bb
  and r = cpBBGetR bb
  and t = cpBBGetT bb in

  let draw_line (xa,ya) (xb,yb) =
    glBegin GL_LINES;
      glVertex2 xa ya;
      glVertex2 xb yb;
    glEnd();
  in

  glLineWidth 1.0;
  glColor3 0.4 1.0 0.7;
  draw_line (l,t) (r,t);
  draw_line (l,b) (r,b);
  draw_line (l,t) (l,b);
  draw_line (r,t) (r,b);
;;


let drawObject shape =
  if draw_bounding_box then
    drawBB (cpShapeGetBody shape) (cpShapeGetBB shape);

  glLineWidth 2.0;
  match cpShapeGetType shape with
  | CP_CIRCLE_SHAPE -> drawCircleShape ~shape;
  | CP_SEGMENT_SHAPE -> drawSegmentShape ~shape;
  | CP_POLY_SHAPE -> drawPolyShape ~shape;
  | _ ->
      Printf.printf("Bad enumeration in drawObject().\n");
;;


let drawCollisions arbiter =
  let arb_arr = cpArbiterGetContacts ~arbiter in

  Array.iter (fun contact ->
      let v = cpContactGetP contact in
      glVertex2  v.cp_x  v.cp_y;
    ) arb_arr;
;;

(* }}} *)

let display() =
  glClear [GL_COLOR_BUFFER_BIT];
  
  List.iter drawObject !shapes_li;
  
  let space = get_global _space in
  
  glBegin GL_POINTS;
    glColor3 0.0 0.0 1.0;
    List.iter (fun body ->
        let p = cpBodyGetPos body in
        glVertex2  p.cp_x  p.cp_y;
      ) !bodies_li;

    glColor3 1.0 0.0 0.0;
    let arbiters = cpSpaceGetArbiters space in
    Array.iter drawCollisions  arbiters;
  glEnd();
  
  glutSwapBuffers();
  incr ticks;
  
  update_funcs.(!demo_index)(!ticks);
;;


let change_demo new_index =
  if 0 <= new_index && new_index < Array.length init_funcs then
  begin
    destroy_funcs.(!demo_index)();
  
    demo_index := new_index;
    ticks := 0;
    init_funcs.(!demo_index)();
  end;
;;
  

let keyboard ~key ~x ~y =
  begin match key with
  | '\027' ->
      destroy_funcs.(!demo_index)();
      exit 0;
  | '1' -> change_demo 0;
  | '2' -> change_demo 1;
  | '3' -> change_demo 2;
  | '4' -> change_demo 3;
  | '5' -> change_demo 4;
  | '6' -> change_demo 5;
  | '7' -> change_demo 6;
  | _ -> ()
  end;  
;;


let rec timercall ~value =
  glutTimerFunc sleep_ticks timercall 0;
    
  glutPostRedisplay();
;;


let idle() =
  glutPostRedisplay();
;;


let initGL() =
  glClearColor 1.0 1.0 1.0 0.0;

  glPointSize 2.5;

  (*
  glEnable GL_LINE_SMOOTH;
  glEnable GL_POINT_SMOOTH;
  *)

  glEnable GL_BLEND;
  glBlendFunc Sfactor.GL_SRC_ALPHA  Dfactor.GL_ONE_MINUS_SRC_ALPHA;
  glHint GL_LINE_SMOOTH_HINT  GL_DONT_CARE;
  glHint GL_POINT_SMOOTH_HINT  GL_DONT_CARE;

  glMatrixMode GL_PROJECTION;
  glLoadIdentity();
  glOrtho (-320.0) (320.0) (-240.0) (240.0) (-1.0) (1.0);
  glTranslate 0.5 0.5 0.0;
;;

let glutStuff() =
  ignore(glutInit Sys.argv);
  
  glutInitDisplayMode [GLUT_DOUBLE; GLUT_RGBA];
  
  glutInitWindowSize 640 480;
  let last = string_of_int(Array.length init_funcs) in
  ignore(glutCreateWindow("Press 1-"^last^" to switch demos"));
  
  initGL();
  
  glutDisplayFunc ~display;
  glutKeyboardFunc ~keyboard;
  glutTimerFunc sleep_ticks timercall 0;
  
  glutMainLoop();
;;


let () =
  cpInitChipmunk();
  
  init_funcs.(!demo_index)();
  
  glutStuff();
;;

(* vim: sw=2 sts=2 ts=2 et fdm=marker
 *)
