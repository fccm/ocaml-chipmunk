#!/usr/bin/env ocaml
(* Copyright (c) 2007 Scott Lembcke
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

(* Converted to OCaml by F. Monnier *)

#directory "+glMLite"
#load "GL.cma"
#load "Glu.cma"
#load "Glut.cma"

open GL
open Glu
open Glut

#directory "+chipmunk"
#load "chipmunk.cma"

open Chipmunk ;;
open Low_level ;;
open OO ;;

(* NOTE: if you haven't read the Chipmunk documentation, you might want to
   do that now. The tutorial doesn't assume that you've read the entire API,
   but you should at least read the overview. Also, I don't deal with memory
   management issues in this tutorial. Keep that in mind. *)

(* This is the terrain height data. *)
let terrain_data = [|
  660.00; 660.00; 660.00; 660.00; 673.21; 688.42; 694.56; 692.55;
  685.40; 676.12; 667.75; 662.45; 658.93; 655.42; 650.17; 641.49;
  627.92; 610.08; 589.01; 565.71; 541.23; 516.58; 492.56; 469.57;
  447.97; 428.13; 410.60; 397.25; 392.66; 394.89; 400.70; 406.82;
  410.93; 413.87; 416.91; 421.30; 428.24; 436.05; 440.41; 437.09;
  421.93; 394.41; 355.57; 308.78; 257.99; 207.18; 160.31; 120.81;
  89.20; 65.17; 48.43; 38.67; 36.68; 45.03; 64.17; 92.26; 128.76;
  173.27; 224.20; 278.84; 334.48; 388.43; 438.31; 483.95; 525.96;
  564.95; 601.54; 633.88; 655.05; 665.87; 667.79; 662.25; 650.01;
  629.92; 604.68; 577.50; 551.55; 529.69; 512.49; 502.04; 500.20;
  502.72; 508.57; 518.31; 531.15; 545.99; 561.70; 577.30; 593.74;
  610.97; 628.13; 644.35; 658.81; 672.13; 684.78; 696.72; 708.00;
  718.65; 728.17; 736.14; 742.62; 747.63; 751.20; 752.58; 750.20;
  743.02; 730.05; 709.98; 682.99; 651.49; 616.61; 579.47; 541.18;
  503.87; 471.12; 444.10; 423.86; 411.44; 407.95; 414.29; 430.28;
  453.64; 482.36; 514.10; 545.66; 577.48; 610.42; 645.32; 682.66;
  719.61; 754.76; 787.26; 816.26; 840.95; 861.10; 876.94; 888.71;
  896.61; 900.84; 900.46; 894.59; 882.69; 864.24; 838.69; 805.77;
  765.56; 718.19; 670.07; 626.07; 586.87; 551.65; 518.20; 484.33;
  447.81; 408.39; 367.51; 324.70; 279.44; 231.25; 181.20; 134.59;
  96.96; 66.40; 40.75; 18.74; 1.97; -8.96; -13.56; -11.33; -2.28;
  11.64; 29.88; 52.04; 78.07; 108.53; 139.94; 171.90; 204.54;
  238.00; 272.25; 305.61; 336.90; 365.19; 389.61; 409.28; 424.38;
  434.79; 438.85; 437.12; 431.08; 422.77; 412.26; 398.92; 382.10;
  361.16; 336.82; 311.06; 285.61; 262.18; 242.50
|]


(* PI *)
let pi = 4.0 *. atan 1.0 ;;

(* Global vars type *)
let init_global() = ref None ;;

(* Retrive global vars *)
let get_global g = match !g with Some v -> v | None -> raise Exit ;;

(* This is the space we will be using for the . *)
let _space = init_global()

(* This is the rigid body we will attach the ground segments to. *)
let _staticBody = init_global()

(* The rigid bodies for the chassis and wheel. They are declared here
   so they can be accessed from the update function. *)
let _chassis = init_global()
let _wheel1 = init_global()
let _wheel2 = init_global()

(* Keep shapes and bodies reachable. *)
let static_shapes_li = ref [] ;;
let active_shapes_li = ref [] ;;

let active_bodies_li = ref [] ;;

(* Chipmunk vectors *)
let cpv x y = {cp_x=x; cp_y=y}
let cpvzero = cpvzero()



(* The init funtion is doing most of the work in this tutorial. We
   create a space and populate it with a bunch of interesting
   objects. *)
let moonBuggy_init() =
  (* We first create a new space *)
  let space = new cp_space in
  _space := Some space;

  (* Next, you'll want to set the properties of the space such as the
     number of iterations to use in the constraint solver, the amount
     of gravity, or the amount of damping. In this case, we'll just
     set the gravity. *)
  space#set_gravity (cpv 0.0 (-900.0));

  (* This step is optional. While you don't have to resize the spatial
  hashes, doing so can greatly increase the speed of the collision
  detection. The first number should be the expected average size of
  the objects you are going to have, the second number is related to
  the number of objects you are putting. In general, if you have more
  objects, you want the number to be bigger, but only to a
  point. Finding good numbers to use here is largely going to be guess
  and check. *)
  space#resize_static_hash 50.0 2000;
  space#resize_active_hash 50.0 100;

  (* This is the rigid body that we will be attaching our ground line
     segments to. We don't want it to move, so we give it an infinite
     mass and moment of inertia. We also aren't going to add it to our
     space. If we did, it would fall under the influence of gravity,
     and the ground would fall with it. *)
  let staticBody = new cp_body infinity infinity in
  _staticBody := Some staticBody;

  (* Number of terrain points. *)
  let terrain_points = Array.length terrain_data in
  (* This loop adds line segments for the terrain. *)
  let rec terrain_loop i a =
    if i >= terrain_points then () else begin
      let b = cpv (float i *. 50.0) terrain_data.(i) in

      (* Collision shapes are attached to rigid bodies. When the rigid
         body moves, the collision shapes attached to it move as
         well. For the ground, we want to attach its collision shapes
         (the line segments) to our static, non-moving body that we've
         created. *)
      let seg = new cp_shape staticBody (SEGMENT_SHAPE(a, b, 0.0)) in
      static_shapes_li := seg :: !static_shapes_li;

      (* After you create a shape, you'll probably want to set some of
         it's properties. Possibilities include elasticity (e), surface
         velocity (surface_v), and friction (u). We'll just set the
         friction. *)
      seg#set_friction 1.0;

      (* Lastly, we need to add it to a space for it to do any
         good. Because the ground never moves, we want to add it to the
         static shapes to allow Chipmunk to cache the collision
         information. Adding the line segments to the active shapes
         would work, but would be slower.  *)
      space#add_static_shape seg;

      terrain_loop (succ i) b
    end
  in
  let a = cpv 0.0 terrain_data.(0) in
  terrain_loop 1 a;

  (* These are the vertexes that will be used to create the buggy's
     chassis shape. You *MUST* specify them in a conterclockwise
     order, and they *MUST* form a convex polygon (no dents). If you
     need a non-convex polygon, simply attach more than one shape to
     the body. *)
  let chassis_verts = [|
    cpv (-18.) (-18.);
    cpv (-18.) ( 18.);
    cpv ( 18.) ( 18.);
    cpv ( 18.) (-18.);
  |] in

  let chassis_mass = 5.0 in

  (* The moment of inertia (usually written simply as 'i') is like the
     mass of an object, but applied to its rotation. An object with a
     higher moment of inertia is harder to spin. Chipmunk has a couple
     of helper functions to help you calculate these. *)
  let chassis_moment = moment_for_poly chassis_mass chassis_verts cpvzero in

  (* Create the rigid body for our buggy with the mass and moment of
     inertia we calculated. *)
  let chassis = new cp_body chassis_mass chassis_moment in
  _chassis := Some chassis;

  active_bodies_li := chassis :: !active_bodies_li;

  (* Like usual, after something, you'll want to set it
     properties. Let's set the buggy's location to be just above the
     start of the terrain. *)
  chassis#set_pos (cpv 100.0 800.0);

  (* Lastly, we need to add the body to a space for it to be
     useful. *)
  space#add_body chassis;


  let wheel_offset_x = 40.0
  and wheel_offset_y = 30.0

  and wheel_radius = 15.0
  and wheel_mass = 1.0 in
  let wheel_moment = moment_for_circle wheel_mass wheel_radius 0.0 cpvzero in

  (* Next, we create our wheels, move them next to the chassis, and
     add them to the space. *)
  let wheel1 = new cp_body wheel_mass wheel_moment
  and wheel2 = new cp_body wheel_mass wheel_moment in
  wheel1#set_pos (cpvadd chassis#get_pos (cpv(-.wheel_offset_x) (-.wheel_offset_y)));
  wheel2#set_pos (cpvadd chassis#get_pos (cpv(  wheel_offset_x) (-.wheel_offset_y)));
  space#add_body wheel1;
  space#add_body wheel2;
  active_bodies_li := wheel1 :: wheel2 :: !active_bodies_li;

  _wheel1 := Some wheel1;
  _wheel2 := Some wheel2;

  (* In order to attach the wheels to the chassis, we need to create
     joints. All of the joints are created slightly differently, but
     the all assume that the bodies they are connecting are already in
     place when you create the joint. Pin joints connect two bodies
     together with a massless rod. We want to attach the joint to the
     wheel at its center so that it rolls nicely, we could attach it
     to the chassis anywhere, though we'll just attach it to the
     center of the chassis as well. *)
  space#add_joint (new cp_joint chassis wheel1 (PIN_JOINT(cpvzero, cpvzero)));
  space#add_joint (new cp_joint chassis wheel2 (PIN_JOINT(cpvzero, cpvzero)));

  (* Now we need to attach collision shapes to the chassis and
     wheels. The shapes themselves contain no useful information to
     you. Normally you'd keep them around simply so that you can
     remomve them from the space, or free them later. In this tutorial
     we won't be removing anything, and we're being lax about memory
     management. So we'll just recycle the same variable. *)

  (* We create a polygon shape for the chassis. *)
  let shape = new cp_shape chassis (POLY_SHAPE(chassis_verts, cpvzero)) in
  shape#set_friction 0.5;
  space#add_shape shape;

  active_shapes_li := shape :: !active_shapes_li;

  (* Now we create some shapes for the wheels *)
  let shape = new cp_shape wheel1 (CIRCLE_SHAPE(wheel_radius, cpvzero)) in
  shape#set_friction 1.5;
  space#add_shape shape;

  active_shapes_li := shape :: !active_shapes_li;

  let shape = new cp_shape wheel2 (CIRCLE_SHAPE(wheel_radius, cpvzero)) in
  shape#set_friction 1.5;
  space#add_shape shape;

  active_shapes_li := shape :: !active_shapes_li;
;;



(* This variable will be used to store the user input. When the mouse
   button is down, the power becomes 1.0. *)
let input_power = ref 0.0 ;;

let moonBuggy_input ~button ~state ~x ~y =
  input_power := match state with GLUT_DOWN -> 1.0 | GLUT_UP -> 0.0;
;;


(* This function is called everytime a frame is drawn. *)
let moonBuggy_update() =
  (* Collision detection isn't amazing in Chipmunk yet. Fast moving
     objects can pass right though eachother if they move to much in a
     single step. To deal with this, you can just make your steps
     smaller and cpSpaceStep() several times. *)
  let substeps = 3 in

  (* This is the actual time step that we will use. *)
  let dt = (1.0 /. 60.0) /. (float substeps) in
  
  let chassis = get_global _chassis
  and wheel1 = get_global _wheel1
  and wheel2 = get_global _wheel2
  and space = get_global _space in

  for i=0 to pred substeps do
    (* In Chipmunk, the forces and torques on a body are not reset
       every step. If you keep accumulating forces on an object, it
       will quickly explode. Comment these lines out to see what I
       mean. This function simply zeros the forces and torques applied
       to a give body. *)
    chassis#reset_forces;
    wheel1#reset_forces;
    wheel2#reset_forces;

    (* We need to calculate how much torque to apply to the wheel. The
       following equation roughly simulates a motor with a top
       speed. *)
    let max_w = -100.0 in
    let torque = 60000.0 *. (min 1.0 ((wheel1#get_a_vel -. !input_power *. max_w) /. max_w)) in

    (* Apply the torque to both the chassis and the wheel in opposite directions. *)
    wheel1#set_torque (wheel1#get_torque +. torque);
    chassis#set_torque (chassis#get_torque -. torque);
    
    (* To simulate the nice soft suspension of the buggy, we apply
       spring forces between the wheels and chassis. This function
       takes a lot of parameters, read the documentation for a
       detailed description. *)
    damped_spring chassis wheel1 (cpv(-40.0) 40.0) cpvzero 70.0 400.0 15.0 dt;
    damped_spring chassis wheel2 (cpv( 40.0) 40.0) cpvzero 70.0 400.0 15.0 dt;
    
    (* Finally, we step the space *)
    space#step dt;
  done;
;;


let sleep_ticks = 16

let ticks = ref 0 ;;

(* {{{ Draw Objects *)

let drawCircle ~x ~y ~r ~a =
  let segs = 15 in
  let coef = 2.0 *. pi /. (float segs) in

  glBegin GL_LINE_LOOP;
    for n=0 to pred segs do
      let rads = (float n) *. coef in
      glVertex2 (r *. cos(rads +. a) +. x)
                (r *. sin(rads +. a) +. y);
    done;
    glVertex2 x y;
  glEnd();
;;


let drawCircleShape ~shape =
  let body = shape#body
  and circle = shape#get_circle_shape
  in
  let c = cpvadd body#get_pos (cpvrotate circle#get_center body#get_rot) in
  drawCircle  c.cp_x  c.cp_y  circle#get_radius  body#get_angle;
;;


let drawSegmentShape ~shape =
  let body = shape#body
  and seg = shape#get_segment_shape
  in
  let a = cpvadd body#get_pos (cpvrotate seg#get_a body#get_rot)
  and b = cpvadd body#get_pos (cpvrotate seg#get_b body#get_rot)
  in

  glBegin GL_LINES;
    glVertex2  a.cp_x  a.cp_y;
    glVertex2  b.cp_x  b.cp_y;
  glEnd();
;;


let drawPolyShape ~shape =
  let body = shape#body
  and poly = shape#get_poly_shape in

  let num = poly#get_num_verts
  and verts = poly#get_verts in

  glBegin GL_LINE_LOOP;
  for i=0 to pred num do
    let v = cpvadd body#get_pos (cpvrotate verts.(i) body#get_rot) in
    glVertex2  v.cp_x  v.cp_y;
  done;
  glEnd();
;;


let drawObject ~obj:shape ~data =
  match shape#kind with
  | CP_CIRCLE_SHAPE -> drawCircleShape(shape);
  | CP_SEGMENT_SHAPE -> drawSegmentShape(shape);
  | CP_POLY_SHAPE -> drawPolyShape(shape);
  | _ ->
      Printf.printf("Bad enumeration in drawObject().\n");
;;

(* }}} *)


let drawCollisions ~obj:arbiter ~data =
  let arb_arr = (get_arbiter_contacts ~arbiter) in

  Array.iter (fun contact ->
      let v = cpContactGetP contact in
      glVertex2  v.cp_x  v.cp_y;
    ) arb_arr;
;;


let display() =
  let chassis = get_global _chassis in
  let x = -. (chassis#get_pos).cp_x
  and y = -. (chassis#get_pos).cp_y in

  glClear [GL_COLOR_BUFFER_BIT];
  glLoadIdentity();
  glTranslate x y 0.0;

  let space = get_global _space in

  glColor3 0.0 0.0 0.0;

  List.iter (fun shape -> drawObject  shape None) !static_shapes_li;
  List.iter (fun shape -> drawObject  shape None) !active_shapes_li;

  let bodies = !active_bodies_li in

  glBegin GL_POINTS;
    glColor3 0.0 0.0 1.0;
    List.iter
      (fun body ->
        let p = body#get_pos in
        glVertex2  p.cp_x  p.cp_y;
      ) bodies;

    glColor3 1.0 0.0 0.0;
    let arbiters = space#get_arbiters  in
    Array.iter (fun arb -> drawCollisions ~obj:arb ~data:None) arbiters;
  glEnd();

  glutSwapBuffers();
  incr ticks;

  moonBuggy_update();
;;


let keyboard ~key ~x ~y =
  match key with '\027' | 'q' | 'Q' -> exit 0;
  | 'z' ->
      static_shapes_li := [];
      active_shapes_li := [];
      active_bodies_li := [];
      let space = get_global _space in
      (* with free_children, no need to use this:
      List.iter (fun shape -> shape#free) !static_shapes_li;
      List.iter (fun shape -> shape#free) !active_shapes_li;
      List.iter (fun body -> body#free) !active_bodies_li;
      *)
      (* frees associated static/active shapes, 
         also active bodies,                                         
         but not static bodies. *)
      space#free_children;
      space#free;
      let staticBody = get_global _staticBody in
      staticBody#free;
      moonBuggy_init();
  | _ -> ()
;;


let rec timer ~value =
  glutTimerFunc ~msecs:sleep_ticks ~timer ~value:0;
  glutPostRedisplay();
;;


let initGL() =
  glClearColor 1.0 1.0 1.0 0.0;

  glPointSize 3.0;

(*  glEnable GL_LINE_SMOOTH; *)
(*  glEnable GL_POINT_SMOOTH; *)
(*  glEnable GL_BLEND; *)
(*  glBlendFunc GL_SRC_ALPHA  GL_ONE_MINUS_SRC_ALPHA; *)
(*  glHint GL_LINE_SMOOTH_HINT  GL_DONT_CARE; *)
(*  glHint GL_POINT_SMOOTH_HINT  GL_DONT_CARE; *)
(*  glLineWidth 1.5; *)

  glMatrixMode GL_PROJECTION;
  glLoadIdentity();
  glOrtho (-1000.0) (1000.0) (-750.0) (750.0) (-1.0) (1.0);
  glScale 2.0 2.0 1.0;
  glMatrixMode GL_MODELVIEW;
;;


let glutStuff() =
  ignore(glutInit Sys.argv);

  glutInitDisplayMode [GLUT_DOUBLE; GLUT_RGBA];

  glutInitWindowSize 640  480;
  ignore(glutCreateWindow "Press mouse button to play");

  initGL();

  glutDisplayFunc ~display;
  glutMouseFunc ~mouse:moonBuggy_input;
  glutKeyboardFunc ~keyboard;
(*  glutIdleFunc ~idle; *)
  glutTimerFunc ~msecs:sleep_ticks ~timer ~value:0;
(*  glutPassiveMotionFunc ~mouse; *)

  glutMainLoop();
;;


let () =
  init_chipmunk();

  moonBuggy_init();

  glutStuff();
;;


(* vim: sw=2 sts=2 ts=2 et fdm=marker
 *)
