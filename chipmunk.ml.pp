(* {{{ COPYING *(

  This file is part of a binding for OCaml to the Chipmunk library.

  Copyright (C) 2008  Florent Monnier  <monnier.florent(_)gmail.com>

  Permission is hereby granted, free of charge, to any person obtaining a
  copy of this software and associated documentation files (the "Software"),
  to deal in the Software without restriction, including without limitation the
  rights to use, copy, modify, merge, publish, distribute, sublicense, and/or
  sell copies of the Software, and to permit persons to whom the Software is
  furnished to do so, subject to the following conditions:

  The above copyright notice and this permission notice shall be included in
  all copies or substantial portions of the Software.

  The Software is provided "as is", without warranty of any kind, express or
  implied, including but not limited to the warranties of merchantability,
  fitness for a particular purpose and noninfringement. In no event shall
  the authors or copyright holders be liable for any claim, damages or other
  liability, whether in an action of contract, tort or otherwise, arising
  from, out of or in connection with the software or the use or other dealings
  in the Software.

)* }}} *)

(** {2 Chipmunk Physics Engine} *)

(** Chipmunk Game Dynamics. Provides fast, easy to use, robust physics. *)

(** {{:http://chipmunk-physics.net/documentation.php}
    Officical documentation of Chipmunk} *)

(* {{{ Low Level module *)
module Low_level = struct
(** {2 Low Level Chipmunk Bindings} *)

external cpInitChipmunk: unit -> unit = "ml_cpInitChipmunk"

external get_compile_version: unit -> int * int * int = "get_chipmunk_compile_version"


type cpVect = {cp_x:float; cp_y:float}

external cpMomentForCircle: m:float -> r1:float -> r2:float -> offset:cpVect -> float = "ml_cpMomentForCircle"
external cpMomentForPoly: m:float -> verts:cpVect array -> offset:cpVect -> float = "ml_cpMomentForPoly"

external cpAreaForPoly: verts:cpVect array -> float = "ml_cpAreaForPoly"


(** {4 Chipmunk Vectors} *)

#ifdef MLI

val vec2d: float * float -> cpVect
val cpv: x:float -> y:float -> cpVect
val cpvzero: unit -> cpVect
val cpvadd : v1:cpVect -> v2:cpVect -> cpVect
val cpvrotate : v1:cpVect -> v2:cpVect -> cpVect
val cpvneg : v:cpVect -> cpVect
val cpvsub : v1:cpVect -> v2:cpVect -> cpVect
val cpvmult : v:cpVect -> s:float -> cpVect
val cpvdot : v1:cpVect -> v2:cpVect -> float
val cpvcross : v1:cpVect -> v2:cpVect -> float
val cpvperp : v:cpVect -> cpVect
val cpvrperp : v:cpVect -> cpVect
val cpvproject : v1:cpVect -> v2:cpVect -> cpVect
val cpvrotate : v1:cpVect -> v2:cpVect -> cpVect
val cpvunrotate : v1:cpVect -> v2:cpVect -> cpVect
val cpvlength : v:cpVect -> float
val cpvlengthsq : v:cpVect -> float
val cpvnormalize : v:cpVect -> cpVect
val cpvforangle : a:float -> cpVect
val cpvtoangle : v:cpVect -> float
val cpvlerp : v1:cpVect -> v2:cpVect -> t:float -> cpVect

#else
(* ML *)

let vec2d (x,y) = { cp_x=x; cp_y=y }
let cpv ~x ~y = { cp_x=x; cp_y=y }

let cpvzero () = { cp_x=0.0; cp_y=0.0 }

let cpvadd ~v1 ~v2 =
  { cp_x = (v1.cp_x +. v2.cp_x);
    cp_y = (v1.cp_y +. v2.cp_y) }
;;

let cpvrotate ~v1 ~v2 =
  { cp_x = (v1.cp_x *. v2.cp_x -. v1.cp_y *. v2.cp_y);
    cp_y = (v1.cp_x *. v2.cp_y +. v1.cp_y *. v2.cp_x) }
;;

let cpvneg ~v = cpv (-. v.cp_x) (-. v.cp_y) ;;

let cpvsub ~v1 ~v2 = cpv (v1.cp_x -. v2.cp_x) (v1.cp_y -. v2.cp_y) ;;

let cpvmult ~v ~s = cpv (v.cp_x *. s) (v.cp_y *. s) ;;

let cpvdot ~v1 ~v2 = (v1.cp_x *. v2.cp_x +. v1.cp_y *. v2.cp_y) ;;

let cpvcross ~v1 ~v2 = (v1.cp_x *. v2.cp_y -. v1.cp_y *. v2.cp_x) ;;

let cpvperp ~v = cpv (-. v.cp_y) (v.cp_x) ;;

let cpvrperp ~v = cpv (v.cp_y) (-. v.cp_x) ;;

let cpvproject ~v1 ~v2 = cpvmult v2 ((cpvdot v1 v2)/.(cpvdot v2 v2)) ;;

let cpvrotate ~v1 ~v2 = cpv (v1.cp_x *. v2.cp_x -. v1.cp_y *. v2.cp_y)
                            (v1.cp_x *. v2.cp_y +. v1.cp_y *. v2.cp_x) ;;

let cpvunrotate ~v1 ~v2 = cpv (v1.cp_x *. v2.cp_x +. v1.cp_y *. v2.cp_y)
                              (v1.cp_y *. v2.cp_x -. v1.cp_x *. v2.cp_y) ;;

let cpvlength ~v = sqrt(cpvdot v v) ;;

let cpvlengthsq ~v = cpvdot v v ;;

let cpvnormalize ~v = cpvmult v (1.0 /. cpvlength v) ;;

let cpvforangle ~a = cpv (cos a) (sin a) ;;

let cpvtoangle ~v = atan2 v.cp_x v.cp_y ;;

let cpvlerp ~v1 ~v2 ~t =
  cpvadd (cpvmult v1 (1.0 -. t)) (cpvmult v2 t) ;;

#endif


(** {3 Chipmunk Bodies} *)

type cpBody
external cpBodyNew: m:float -> i:float -> cpBody = "ml_cpBodyNew"

external cpBodyGetUserData: cpBody -> int = "ml_cpBodyGetUserData"

external cpBodySetUserData: cpBody -> int -> unit = "ml_cpBodySetUserData"


(** {3 Chipmunk Spaces} *)

type cpSpace
external cpSpaceNew: unit -> cpSpace = "ml_cpSpaceNew"


(** {3 Chipmunk Shapes} *)

type cpShape
external cpSegmentShapeNew: body:cpBody -> a:cpVect -> b:cpVect -> r:float -> cpShape = "ml_cpSegmentShapeNew"
(** [r] is the thickness *)

external cpPolyShapeNew: body:cpBody -> verts:cpVect array -> offset:cpVect -> cpShape = "ml_cpPolyShapeNew"
external cpCircleShapeNew: body:cpBody -> radius:float -> offset:cpVect -> cpShape = "ml_cpCircleShapeNew"
external cpBoxShapeNew: body:cpBody -> width:float -> height:float -> cpShape = "ml_cpBoxShapeNew"

external cpShapeGetBody: shape:cpShape -> cpBody = "ml_cpShapeGetBody"


type cpCircleShape
type cpSegmentShape
type cpPolyShape
external cpCircleShape_of_cpShape: shape:cpShape -> cpCircleShape = "%identity"
external cpSegmentShape_of_cpShape: shape:cpShape -> cpSegmentShape = "%identity"
external cpPolyShape_of_cpShape: shape:cpShape -> cpPolyShape = "%identity"
(** unsafe convertion functions, use the high level module for a safe interface,
    or check type with [cpShapeGetType] before. *)

type cpShapeType =
  | CP_CIRCLE_SHAPE
  | CP_SEGMENT_SHAPE
  | CP_POLY_SHAPE
  | CP_NUM_SHAPES

external cpShapeGetType: shape:cpShape -> cpShapeType = "ml_cpShapeGetType"

type cpSegmentQueryInfo = {
  shape : cpShape;
  (** The shape that was hit, NULL if no collision occured. *)
  t : float;
  (** The normalized distance along the query segment in the range [0, 1]. *)
  n : cpVect;
  (** The normal of the surface hit. *)
}

external cpShapeSegmentQuery: shape:cpShape -> a:cpVect -> b:cpVect ->
  cpSegmentQueryInfo option = "ml_cpShapeSegmentQuery"

external cpSegmentQueryHitPoint: start:cpVect -> end_:cpVect -> info:cpSegmentQueryInfo -> cpVect
  = "ml_cpSegmentQueryHitPoint"

external cpSegmentQueryHitDist: start:cpVect -> end_:cpVect -> info:cpSegmentQueryInfo -> float
  = "ml_cpSegmentQueryHitDist"


(** Debug *)
external cpSpaceDump: space:cpSpace -> unit = "ml_cpSpaceDump"
external cpBodyDump: body:cpBody -> unit = "ml_cpBodyDump"

(** Chipmunk Bounding Box *)

type cpBB
external cpBBNew: l:float -> b:float -> r:float -> t:float -> cpBB = "ml_cpBBNew"
external cpBBFree: bb:cpBB -> unit = "ml_cpBBFree"

external cpBBIntersects: a:cpBB -> b:cpBB -> bool = "ml_cpBBIntersects"
external cpBBContainsBB: bb:cpBB -> other:cpBB -> bool = "ml_cpBBContainsBB"
external cpBBContainsVect: bb:cpBB -> v:cpVect -> bool = "ml_cpBBContainsVect"

external cpBBClampVect: bb:cpBB -> v:cpVect -> cpVect = "ml_cpBBClampVect"
external cpBBWrapVect: bb:cpBB -> v:cpVect -> cpVect = "ml_cpBBWrapVect"

external cpShapeGetBB: shape:cpShape -> cpBB = "ml_cpShapeGetBB"  (* TODO: test this function *)


(** {3 Chipmunk Joints} *)

type cpJointType =
  | CP_PIN_JOINT
  | CP_SLIDE_JOINT
  | CP_PIVOT_JOINT
  | CP_GROOVE_JOINT

type cpConstraint
type cpJoint

type cpPinJoint
type cpSlideJoint
type cpPivotJoint
type cpGrooveJoint

external cpPinJoint_of_cpJoint: joint:cpJoint -> cpPinJoint = "%identity"
external cpSlideJoint_of_cpJoint: joint:cpJoint -> cpSlideJoint = "%identity"
external cpPivotJoint_of_cpJoint: joint:cpJoint -> cpPivotJoint = "%identity"
external cpGrooveJoint_of_cpJoint: joint:cpJoint -> cpGrooveJoint = "%identity"
(** unsafe convertion functions, use the high level module for a safe interface.
    There is no low-level function to check the joint type. *)


(** {3 Chipmunk Contacts} *)

type cpContact



(** {4 Iterating} *)

type cpArbiter

external cpArbiterGetShapePA: arbiter:cpArbiter -> cpShape = "ml_cpArbiterGetShapePA"
external cpArbiterGetShapePB: arbiter:cpArbiter -> cpShape = "ml_cpArbiterGetShapePB"
external cpArbiterGetShapes: arbiter:cpArbiter -> cpShape * cpShape = "ml_cpArbiterGetShapes"


(** {5 Generated Part} *)

type cpVectArray = cpVect array

#include "chipmunk.gen.ml"


#ifdef MLI
val cpSpaceAddCollisionHandler : space:cpSpace -> a:int -> b:int -> data:string -> unit
#else
(* ML *)
let cpSpaceAddCollisionHandler =
 my_cpSpaceAddCollisionHandler ;;
#endif


end
(* }}} *)

#ifdef MLI

(** {2 High Level, Object Oriented, Interface} *)
(* {{{ generated sig *)
module OO :
  sig
    val init_chipmunk : unit -> unit
    val moment_for_circle :
      m:float -> r1:float -> r2:float -> offset:Low_level.cpVect -> float
(** Calculate the moment of inertia for a circle with the given mass, inner and
    outer radii, and offset. *)
    val moment_for_poly :
      m:float ->
      verts:Low_level.cpVect array -> offset:Low_level.cpVect -> float
(** Calculate the moment of inertia for a polygon with the given mass, vertexes,
    and offset. verts should be an array of cpVect with a counterclockwise
    winding, offset should be a cpVect. *)
    class virtual cp_body_virt :
      Low_level.cpBody ->
      object
        val body : Low_level.cpBody
        method virtual apply_force :
          f:Low_level.cpVect -> r:Low_level.cpVect -> unit
        method virtual apply_impulse :
          j:Low_level.cpVect -> r:Low_level.cpVect -> unit
        method virtual body : Low_level.cpBody
        method virtual free : unit
        method virtual get_a_vel : float
        method virtual get_angle : float
        method virtual get_force : Low_level.cpVect
        method virtual get_mass : float
        method virtual get_moment : float
        method virtual get_pos : Low_level.cpVect
        method virtual get_rot : Low_level.cpVect
        method virtual get_torque : float
        method virtual get_vel : Low_level.cpVect
        method virtual local2world : v:Low_level.cpVect -> Low_level.cpVect
        method virtual reset_forces : unit
        method virtual set_a_vel : w:float -> unit
        method virtual set_angle : a:float -> unit
        method virtual set_force : f:Low_level.cpVect -> unit
        method virtual set_mass : m:float -> unit
        method virtual set_moment : i:float -> unit
        method virtual set_pos : p:Low_level.cpVect -> unit
        (*
        method virtual set_rot : rot:Low_level.cpVect -> unit
        *)
        method virtual set_vel_lim     : v_limit:float -> unit
        method virtual set_ang_vel_lim : w_limit:float -> unit
        method virtual set_torque : t:float -> unit
        method virtual set_vel : v:Low_level.cpVect -> unit
        method virtual update_position : dt:float -> unit
        method virtual update_velocity :
          gravity:Low_level.cpVect -> damping:float -> dt:float -> unit
        method virtual world2local : v:Low_level.cpVect -> Low_level.cpVect
      end
    class cp_body :
      m:float ->
      i:float ->
      object
        val body : Low_level.cpBody
        method apply_force : f:Low_level.cpVect -> r:Low_level.cpVect -> unit
        method apply_impulse :
          j:Low_level.cpVect -> r:Low_level.cpVect -> unit
        method body : Low_level.cpBody
        method free : unit
        method get_a_vel : float
        method get_angle : float
        method get_force : Low_level.cpVect
        method get_mass : float
        method get_moment : float
        method get_pos : Low_level.cpVect
        method get_rot : Low_level.cpVect
        method get_torque : float
        method get_vel : Low_level.cpVect
        method local2world : v:Low_level.cpVect -> Low_level.cpVect
        method reset_forces : unit
        method set_a_vel : w:float -> unit
        method set_angle : a:float -> unit
        method set_force : f:Low_level.cpVect -> unit
        method set_mass : m:float -> unit
        method set_moment : i:float -> unit
        method set_pos : p:Low_level.cpVect -> unit
        (*
        method set_rot : rot:Low_level.cpVect -> unit
        *)
        method set_vel_lim     : v_limit:float -> unit
        method set_ang_vel_lim : w_limit:float -> unit
        method set_torque : t:float -> unit
        method set_vel : v:Low_level.cpVect -> unit
        method update_position : dt:float -> unit
        method update_velocity :
          gravity:Low_level.cpVect -> damping:float -> dt:float -> unit
        method world2local : v:Low_level.cpVect -> Low_level.cpVect
      end

    val to_cp_body : body:Low_level.cpBody -> cp_body_virt

    exception Wrong_joint_kind

(** parameters to create a new [cp_joint]. *)
    type joint_kind =
        PIN_JOINT of Low_level.cpVect * Low_level.cpVect                       (** anchor1, anchor2 *)
      | SLIDE_JOINT of Low_level.cpVect * Low_level.cpVect * float * float     (** anchor1, anchor2, min, max *)
      | PIVOT_JOINT of Low_level.cpVect                                        (** pivot *)
      | GROOVE_JOINT of Low_level.cpVect * Low_level.cpVect * Low_level.cpVect (** groove_a, groove_b, anchor2 *)
(**
  [anchor1] and [anchor2] are the anchor points on those bodies.

  [anchor1] and [anchor2] are the anchor points on those bodies, and [min] and
  [max] define the allowed distances of the anchor points.

  [pivot] is the point in world coordinates of the pivot.
  Because the pivot location is given in world coordinates, you must have the
  bodies moved into the correct positions already.

  The groove goes from [groove_a] to [groove_b] on body [a], and the pivot is
  attached to [anchor2] on body [b]. All coordinates are body local.
*)


(** [a] and [b] are the two bodies to connect. *)
    class cp_joint :
      a:cp_body_virt ->
      b:cp_body_virt ->
      kind:joint_kind ->
      object
        val constr : Low_level.cpConstraint
        val kind : Low_level.cpJointType
        method free : unit
        (*
        method get_groove_joint : cp_groove_joint
        method get_pin_joint : cp_pin_joint
        method get_pivot_joint : cp_pivot_joint
        method get_slide_joint : cp_slide_joint
        *)
        method get_constraint : Low_level.cpConstraint

        method is_groove_joint : bool
        method is_pin_joint : bool
        method is_pivot_joint : bool
        method is_slide_joint : bool
        method constr : Low_level.cpConstraint
        method kind : Low_level.cpJointType
      end


    val reset_shape_id_counter : unit -> unit
    exception Wrong_shape_kind
    class cp_circle_shape :
      shape:< shape : Low_level.cpShape; .. > ->
      object
        val circle_shape : Low_level.cpCircleShape
        method get_center : Low_level.cpVect
        method get_radius : float
        method set_center : c:Low_level.cpVect -> unit
        method set_radius : r:float -> unit
      end
    class cp_segment_shape :
      shape:< shape : Low_level.cpShape; .. > ->
      object
        val segment_shape : Low_level.cpSegmentShape
        method get_a : Low_level.cpVect
        method get_b : Low_level.cpVect
        method get_norm : Low_level.cpVect
        method get_t_a : Low_level.cpVect
        method get_t_b : Low_level.cpVect
        method get_t_norm : Low_level.cpVect
        method get_thick : float
        method set_a : a:Low_level.cpVect -> unit
        method set_b : b:Low_level.cpVect -> unit
        method set_norm : n:Low_level.cpVect -> unit
        method set_t_a : ta:Low_level.cpVect -> unit
        method set_t_b : tb:Low_level.cpVect -> unit
        method set_t_norm : tn:Low_level.cpVect -> unit
        method set_thick : r:float -> unit
      end
    class cp_poly_shape :
      shape:< shape : Low_level.cpShape; .. > ->
      object
        val poly : Low_level.cpPolyShape
        method get_num_verts : int
        method get_t_verts : Low_level.cpVectArray
        method get_verts : Low_level.cpVectArray
      end
    class virtual cp_shape_virt :
      Low_level.cpShape ->
      object
        val shape : Low_level.cpShape
        method virtual body : cp_body_virt
        method virtual free : unit
        method virtual get_circle_shape : cp_circle_shape
        method virtual get_elasticity : float
        method virtual get_friction : float
        method virtual get_layers : int
        method virtual get_poly_shape : cp_poly_shape
        method virtual get_segment_shape : cp_segment_shape
        method virtual get_surface_vel : Low_level.cpVect
        (*
        method virtual is_circle_shape : bool
        method virtual is_poly_shape : bool
        method virtual is_segment_shape : bool
        method virtual kind : Low_level.cpShapeType
        *)
        (*
        method virtual set_collision_type : collision_type:int -> unit
        method virtual get_collision_type : int
        *)
        (*
        method virtual get_group : int
        method virtual set_group : group:int -> unit
        *)
        method virtual set_elasticity : e:float -> unit
        method virtual set_friction : u:float -> unit
        method virtual set_layers : layers:int -> unit
        method virtual set_surface_vel : surface_vel:Low_level.cpVect -> unit
        method virtual shape : Low_level.cpShape
      end
(** parameter for each shape kind *)
    type shape_kind =
        CIRCLE_SHAPE of float * Low_level.cpVect                       (** radius, center *)
      | SEGMENT_SHAPE of Low_level.cpVect * Low_level.cpVect * float   (** a, b, thickness *)
      | POLY_SHAPE of Low_level.cpVect array * Low_level.cpVect        (** verts, offset *)
    class cp_shape :
      body:cp_body_virt ->
      kind:shape_kind ->
      object
        val shape : Low_level.cpShape
        method body : cp_body_virt
        method free : unit
        method get_circle_shape : cp_circle_shape
        (*
        method get_collision_type : int
        *)
        method get_elasticity : float
        method get_friction : float
        method get_layers : int
        method get_poly_shape : cp_poly_shape
        method get_segment_shape : cp_segment_shape
        method get_surface_vel : Low_level.cpVect
    (** Surface velocity used when solving for friction. *)
        (*
        method is_circle_shape : bool
        method is_poly_shape : bool
        method is_segment_shape : bool
        method kind : Low_level.cpShapeType
        *)
        (*
        method set_collision_type : collision_type:int -> unit
        *)
    (** User defined collision type for the shape. *)
        method set_elasticity : e:float -> unit
    (** Coefficient of restitution. (elasticity) *)
        method set_friction : u:float -> unit
    (** Coefficient of friction. *)
        (*
        method get_group : int
        method set_group : group:int -> unit
    (** User defined collision group for the shape. *)
        *)
        method set_layers : layers:int -> unit
    (** User defined layer bitmask for the shape. *)
        method set_surface_vel : surface_vel:Low_level.cpVect -> unit
        method shape : Low_level.cpShape
      end
    val to_cp_shape : shape:Low_level.cpShape -> cp_shape_virt
    class cp_space :
      object
        val space : Low_level.cpSpace
        method add_body : body:cp_body_virt -> unit
        method add_shape : shape:cp_shape_virt -> unit
    (** Add the given shape to the space's active spatial hash. Shapes attached to
        moving bodies should be added here as they will be rehashed on every call
        to [space#step]. *)
        method add_static_shape : shape:cp_shape_virt -> unit
        method add_constraint : constr:Low_level.cpConstraint -> unit
    (** Add the given shape to the space's static spatial hash. *)
        method free : unit
        method get_damping : float
        method get_gravity : Low_level.cpVect
        method get_iterations : int
        method remove_body : body:cp_body_virt -> unit
        method remove_shape : shape:cp_shape_virt -> unit
        method remove_static_shape : shape:cp_shape_virt -> unit
        method remove_constraint : constr:Low_level.cpConstraint -> unit
        method set_damping : damping:float -> unit
    (** The amount of damping to apply to the system when updating. *)
        method set_gravity : gravity:Low_level.cpVect -> unit
    (** The amount of gravity in the system. *)
        method set_iterations : iterations:int -> unit
    (** The number of iterations to use when solving constraints. (collisions and joints) *)
        method space : Low_level.cpSpace
        method step : dt:float -> unit
    (** Move the space forward by dt seconds. Using a fixed size time step is
        {i highly} recommended for efficiency of the contact persistence algorithm. *)

    (**
      {b Notes:}

      When removing objects from the space, make sure you remove any other objects that
      reference it. For instance, when you remove a body, remove the joints and shapes
      attached to it.

      The number of iterations, and the size of the time step determine the quality of
      the simulation. More iterations, or smaller time steps increase the quality.
    *)
      end

    val damped_spring :
      a:cp_body_virt ->
      b:cp_body_virt ->
      anchr1:Low_level.cpVect ->
      anchr2:Low_level.cpVect ->
      rest_length:float -> stiffness:float -> damping:float -> Low_level.cpConstraint
(** Apply a spring force between bodies a and b at anchors anchr1 and anchr2
    respectively. k is the spring constant (force/distance), rlen is the rest
    length of the spring, dmp is the damping constant (force/velocity), and dt
    is the time step to apply the force over. Note: not solving the damping
    forces in the impulse solver causes problems with large damping values. This
    function will eventually be replaced by a new constraint (joint) type. *)

  end
(* }}} end of module OO *)

#else
(* ML *)

module OO = struct
open Low_level

let init_chipmunk = cpInitChipmunk ;;

let moment_for_circle = cpMomentForCircle ;;
(** Calculate the moment of inertia for a circle with the given mass, inner and
    outer radii, and offset. *)

let moment_for_poly = cpMomentForPoly ;;
(** Calculate the moment of inertia for a polygon with the given mass, vertexes,
    and offset. verts should be an array of cpVect with a counterclockwise
    winding, offset should be a cpVect. *)


(* {{{ Body *)

(* {{{ virtual cp_body *)

class virtual cp_body_virt (_body : Low_level.cpBody) =
  object
    val body = _body
    method virtual apply_force : f:Low_level.cpVect -> r:Low_level.cpVect -> unit
    method virtual apply_impulse : j:Low_level.cpVect -> r:Low_level.cpVect -> unit
    method virtual body : Low_level.cpBody
    method virtual free : unit
    method virtual get_a_vel : float
    method virtual get_angle : float
    method virtual get_force : Low_level.cpVect
    method virtual get_mass : float
    method virtual get_moment : float
    method virtual get_pos : Low_level.cpVect
    method virtual get_rot : Low_level.cpVect
    method virtual get_torque : float
    method virtual get_vel : Low_level.cpVect
    method virtual local2world : v:Low_level.cpVect -> Low_level.cpVect
    method virtual reset_forces : unit
    method virtual set_a_vel : w:float -> unit
    method virtual set_angle : a:float -> unit
    method virtual set_force : f:Low_level.cpVect -> unit
    method virtual set_mass : m:float -> unit
    method virtual set_moment : i:float -> unit
    method virtual set_pos : p:Low_level.cpVect -> unit
    (*
    method virtual set_rot : rot:Low_level.cpVect -> unit
    *)
    method virtual set_vel_lim     : v_limit:float -> unit
    method virtual set_ang_vel_lim : w_limit:float -> unit
    method virtual set_torque : t:float -> unit
    method virtual set_vel : v:Low_level.cpVect -> unit
    method virtual update_position : dt:float -> unit
    method virtual update_velocity : gravity:Low_level.cpVect -> damping:float -> dt:float -> unit
    method virtual world2local : v:Low_level.cpVect -> Low_level.cpVect
  end

(* }}} *)

class cp_body ~m ~i =
  object
  inherit cp_body_virt (cpBodyNew ~m ~i)

    method body = body

    method free = cpBodyFree body

    method update_velocity    = cpBodyUpdateVelocity ~body
    method update_position    = cpBodyUpdatePosition ~body
    method local2world        = cpBodyLocal2World ~body
    method world2local        = cpBodyWorld2Local ~body
    method apply_impulse      = cpBodyApplyImpulse ~body
    method reset_forces       = cpBodyResetForces ~body
    method apply_force        = cpBodyApplyForce ~body

    method set_mass           = cpBodySetMass ~body
    method set_moment         = cpBodySetMoment ~body
    method set_pos            = cpBodySetPos ~body
    method set_vel            = cpBodySetVel ~body
    method set_force          = cpBodySetForce ~body
    method set_angle          = cpBodySetAngle ~body
    method set_a_vel          = cpBodySetAngVel ~body
    method set_torque         = cpBodySetTorque ~body
    (*
    method set_rot            = cpBodySetRot ~body
    *)
    method set_vel_lim        = cpBodySetVelLimit ~body
    method set_ang_vel_lim    = cpBodySetAngVelLimit ~body
    method get_mass           = cpBodyGetMass ~body
    method get_moment         = cpBodyGetMoment ~body
    method get_pos            = cpBodyGetPos ~body
    method get_vel            = cpBodyGetVel ~body
    method get_force          = cpBodyGetForce ~body
    method get_angle          = cpBodyGetAngle ~body
    method get_a_vel          = cpBodyGetAngVel ~body
    method get_torque         = cpBodyGetTorque ~body
    method get_rot            = cpBodyGetRot ~body
  end
;;


let to_cp_body ~body =
  object
  inherit cp_body_virt (body)

    method body = body

    method free = cpBodyFree body

    method update_velocity    = cpBodyUpdateVelocity ~body
    method update_position    = cpBodyUpdatePosition ~body
    method local2world        = cpBodyLocal2World ~body
    method world2local        = cpBodyWorld2Local ~body
    method apply_impulse      = cpBodyApplyImpulse ~body
    method reset_forces       = cpBodyResetForces ~body
    method apply_force        = cpBodyApplyForce ~body

    method set_mass           = cpBodySetMass ~body
    method set_moment         = cpBodySetMoment ~body
    method set_pos            = cpBodySetPos ~body
    method set_vel            = cpBodySetVel ~body
    method set_force          = cpBodySetForce ~body
    method set_angle          = cpBodySetAngle ~body
    method set_a_vel          = cpBodySetAngVel ~body
    method set_torque         = cpBodySetTorque ~body
    (*
    method set_rot            = cpBodySetRot ~body
    *)
    method set_vel_lim        = cpBodySetVelLimit ~body
    method set_ang_vel_lim    = cpBodySetAngVelLimit ~body
    method get_mass           = cpBodyGetMass ~body
    method get_moment         = cpBodyGetMoment ~body
    method get_pos            = cpBodyGetPos ~body
    method get_vel            = cpBodyGetVel ~body
    method get_force          = cpBodyGetForce ~body
    method get_angle          = cpBodyGetAngle ~body
    method get_a_vel          = cpBodyGetAngVel ~body
    method get_torque         = cpBodyGetTorque ~body
    method get_rot            = cpBodyGetRot ~body
  end
;;

(* }}} *)
(* {{{ Joints *)

exception Wrong_joint_kind ;;

(*
class cp_pin_joint ~joint =
  object
    val pin_joint =
      if not joint#is_pin_joint then raise Wrong_joint_kind else
      cpPinJoint_of_cpJoint ~joint:joint#joint

    method set_anchor1 = cpPinJointSetAnchor1 ~pin_joint
    method set_anchor2 = cpPinJointSetAnchor2 ~pin_joint
    method set_dist    = cpPinJointSetDist ~pin_joint
    method set_r1      = cpPinJointSetR1 ~pin_joint
    method set_r2      = cpPinJointSetR2 ~pin_joint
    method set_vect    = cpPinJointSetVect ~pin_joint
    method set_n_mass  = cpPinJointSetNMass ~pin_joint
    method set_j_n_acc = cpPinJointSetJNAcc ~pin_joint
    method set_bias    = cpPinJointSetBias ~pin_joint

    method get_anchor1 = cpPinJointGetAnchor1 ~pin_joint
    method get_anchor2 = cpPinJointGetAnchor2 ~pin_joint
    method get_dist    = cpPinJointGetDist ~pin_joint
    method get_r1      = cpPinJointGetR1 ~pin_joint
    method get_r2      = cpPinJointGetR2 ~pin_joint
    method get_vect    = cpPinJointGetVect ~pin_joint
    method get_n_mass  = cpPinJointGetNMass ~pin_joint
    method get_j_n_acc = cpPinJointGetJNAcc ~pin_joint
    method get_bias    = cpPinJointGetBias ~pin_joint
  end
*)

(*
class cp_slide_joint ~joint =
  object
    val slide_joint =
      if not joint#is_slide_joint then raise Wrong_joint_kind else
      cpSlideJoint_of_cpJoint ~joint:joint#joint

    method set_anchor1 = cpSlideJointSetAnchor1 ~slide_joint
    method set_anchor2 = cpSlideJointSetAnchor2 ~slide_joint
    method set_min     = cpSlideJointSetMin ~slide_joint
    method set_max     = cpSlideJointSetMax ~slide_joint
    method set_r1      = cpSlideJointSetR1 ~slide_joint
    method set_r2      = cpSlideJointSetR2 ~slide_joint
    method set_vect    = cpSlideJointSetVect ~slide_joint
    method set_n_mass  = cpSlideJointSetNMass ~slide_joint
    method set_j_n_acc = cpSlideJointSetJNAcc ~slide_joint
    method set_j_bias  = cpSlideJointSetJBias ~slide_joint
    method set_bias    = cpSlideJointSetBias ~slide_joint

    method get_anchor1 = cpSlideJointGetAnchor1 ~slide_joint
    method get_anchor2 = cpSlideJointGetAnchor2 ~slide_joint
    method get_min     = cpSlideJointGetMin ~slide_joint
    method get_max     = cpSlideJointGetMax ~slide_joint
    method get_r1      = cpSlideJointGetR1 ~slide_joint
    method get_r2      = cpSlideJointGetR2 ~slide_joint
    method get_vect    = cpSlideJointGetVect ~slide_joint
    method get_n_mass  = cpSlideJointGetNMass ~slide_joint
    method get_j_n_acc = cpSlideJointGetJNAcc ~slide_joint
    method get_j_bias  = cpSlideJointGetJBias ~slide_joint
    method get_bias    = cpSlideJointGetBias ~slide_joint
  end
*)

(*
class cp_pivot_joint ~joint =
  object
    val pivot_joint =
      if not joint#is_pivot_joint then raise Wrong_joint_kind else
      cpPivotJoint_of_cpJoint ~joint:joint#joint

    method set_anchor1 = cpPivotJointSetAnchor1 ~pivot_joint
    method set_anchor2 = cpPivotJointSetAnchor2 ~pivot_joint
    method set_r1      = cpPivotJointSetR1 ~pivot_joint
    method set_r2      = cpPivotJointSetR2 ~pivot_joint
    method set_k1      = cpPivotJointSetK1 ~pivot_joint
    method set_k2      = cpPivotJointSetK2 ~pivot_joint
    method set_j_acc   = cpPivotJointSetJAcc ~pivot_joint
    method set_j_bias  = cpPivotJointSetJBias ~pivot_joint
    method set_bias    = cpPivotJointSetBias ~pivot_joint

    method get_anchor1 = cpPivotJointGetAnchor1 ~pivot_joint
    method get_anchor2 = cpPivotJointGetAnchor2 ~pivot_joint
    method get_r1      = cpPivotJointGetR1 ~pivot_joint
    method get_r2      = cpPivotJointGetR2 ~pivot_joint
    method get_k1      = cpPivotJointGetK1 ~pivot_joint
    method get_k2      = cpPivotJointGetK2 ~pivot_joint
    method get_j_acc   = cpPivotJointGetJAcc ~pivot_joint
    method get_j_bias  = cpPivotJointGetJBias ~pivot_joint
    method get_bias    = cpPivotJointGetBias ~pivot_joint
  end
*)

(*
class cp_groove_joint ~joint =
  object
    val groove_joint =
      if not joint#is_groove_joint then raise Wrong_joint_kind else
      cpGrooveJoint_of_cpJoint ~joint:joint#joint

    method set_grv_n   = cpGrooveJointSetGrvN ~groove_joint
    method set_grv_a   = cpGrooveJointSetGrvA ~groove_joint
    method set_grv_b   = cpGrooveJointSetGrvB ~groove_joint
    method set_anchor2 = cpGrooveJointSetAnchor2 ~groove_joint
    method set_grv_tn  = cpGrooveJointSetGrvTn ~groove_joint
    method set_clamp   = cpGrooveJointSetClamp ~groove_joint
    method set_r1      = cpGrooveJointSetR1 ~groove_joint
    method set_r2      = cpGrooveJointSetR2 ~groove_joint
    method set_k1      = cpGrooveJointSetK1 ~groove_joint
    method set_k2      = cpGrooveJointSetK2 ~groove_joint
    method set_j_acc   = cpGrooveJointSetJAcc ~groove_joint
    method set_j_bias  = cpGrooveJointSetJBias ~groove_joint
    method set_bias    = cpGrooveJointSetBias ~groove_joint

    method get_grv_n   = cpGrooveJointGetGrvN ~groove_joint
    method get_grv_a   = cpGrooveJointGetGrvA ~groove_joint
    method get_grv_b   = cpGrooveJointGetGrvB ~groove_joint
    method get_anchor2 = cpGrooveJointGetAnchor2 ~groove_joint
    method get_grv_tn  = cpGrooveJointGetGrvTn ~groove_joint
    method get_clamp   = cpGrooveJointGetClamp ~groove_joint
    method get_r1      = cpGrooveJointGetR1 ~groove_joint
    method get_r2      = cpGrooveJointGetR2 ~groove_joint
    method get_k1      = cpGrooveJointGetK1 ~groove_joint
    method get_k2      = cpGrooveJointGetK2 ~groove_joint
    method get_j_acc   = cpGrooveJointGetJAcc ~groove_joint
    method get_j_bias  = cpGrooveJointGetJBias ~groove_joint
    method get_bias    = cpGrooveJointGetBias ~groove_joint
  end
*)


(** parameters to create a new [cp_joint]. *)
type joint_kind =
  | PIN_JOINT    of cpVect * cpVect                 (** anchor1, anchor2 *)
  | SLIDE_JOINT  of cpVect * cpVect * float * float (** anchor1, anchor2, min, max *)
  | PIVOT_JOINT  of cpVect                          (** pivot *)
  | GROOVE_JOINT of cpVect * cpVect * cpVect        (** groove_a, groove_b, anchor2 *)
(**
  [anchor1] and [anchor2] are the anchor points on those bodies.

  [anchor1] and [anchor2] are the anchor points on those bodies, and [min] and
  [max] define the allowed distances of the anchor points.

  [pivot] is the point in world coordinates of the pivot.
  Because the pivot location is given in world coordinates, you must have the
  bodies moved into the correct positions already.

  The groove goes from [groove_a] to [groove_b] on body [a], and the pivot is
  attached to [anchor2] on body [b]. All coordinates are body local.
*)


(** [a] and [b] are the two bodies to connect. *)
class cp_joint ~a:(_a :cp_body_virt) ~b:(_b :cp_body_virt) ~kind:_kind =
  let a = _a#body
  and b = _b#body
  in
  object (self)
    val kind =
      match _kind with
      | PIN_JOINT    _ -> CP_PIN_JOINT
      | SLIDE_JOINT  _ -> CP_SLIDE_JOINT
      | PIVOT_JOINT  _ -> CP_PIVOT_JOINT
      | GROOVE_JOINT _ -> CP_GROOVE_JOINT

    val constr =
      match _kind with
      | PIN_JOINT    (anchr1, anchr2)             -> cpPinJointNew ~a ~b ~anchr1 ~anchr2
      | SLIDE_JOINT  (anchr1, anchr2, min, max)   -> cpSlideJointNew ~a ~b ~anchr1 ~anchr2 ~min ~max
      | PIVOT_JOINT  (pivot)                      -> cpPivotJointNew ~a ~b ~pivot
      | GROOVE_JOINT (groove_a, groove_b, anchr2) -> cpGrooveJointNew ~a ~b ~groove_a ~groove_b ~anchr2

    method free = cpConstraintFree ~constr
    method kind = kind
    method constr = constr

    (*
    method get_pin_joint    = new cp_pin_joint ~joint:self
    method get_slide_joint  = new cp_slide_joint ~joint:self
    method get_pivot_joint  = new cp_pivot_joint ~joint:self
    method get_groove_joint = new cp_groove_joint ~joint:self
    *)
    method get_constraint = constr

    method is_pin_joint    = (kind = CP_PIN_JOINT)
    method is_slide_joint  = (kind = CP_SLIDE_JOINT)
    method is_pivot_joint  = (kind = CP_PIVOT_JOINT)
    method is_groove_joint = (kind = CP_GROOVE_JOINT)
  end
;;

(* }}} *)
(* {{{ Shapes *)

let reset_shape_id_counter = cpResetShapeIdCounter ;;


exception Wrong_shape_kind ;;

class cp_circle_shape ~shape =
  object
    val circle_shape =
      let shape = shape#shape in
      (*
      if cpShapeGetType ~shape <> CP_CIRCLE_SHAPE then raise Wrong_shape_kind;
      *)
      cpCircleShape_of_cpShape ~shape

    method set_center = cpCircleShapeSetCenter ~circle_shape
    method set_radius = cpCircleShapeSetRadius ~circle_shape

    method get_center = cpCircleShapeGetCenter ~circle_shape
    method get_radius = cpCircleShapeGetRadius ~circle_shape
  end
;;

class cp_segment_shape ~shape =
  object
    val segment_shape =
      let shape = shape#shape in
      (*
      if cpShapeGetType ~shape <> CP_SEGMENT_SHAPE then raise Wrong_shape_kind;
      *)
      cpSegmentShape_of_cpShape ~shape

    method set_a      = cpSegmentShapeSetA ~segment_shape
    method set_b      = cpSegmentShapeSetB ~segment_shape
    method set_norm   = cpSegmentShapeSetNorm ~segment_shape
    method set_thick  = cpSegmentShapeSetThickness ~segment_shape
    method set_t_a    = cpSegmentShapeSetTA ~segment_shape
    method set_t_b    = cpSegmentShapeSetTB ~segment_shape
    method set_t_norm = cpSegmentShapeSetTNorm ~segment_shape

    method get_a      = cpSegmentShapeGetA ~segment_shape
    method get_b      = cpSegmentShapeGetB ~segment_shape
    method get_norm   = cpSegmentShapeGetNorm ~segment_shape
    method get_thick  = cpSegmentShapeGetThickness ~segment_shape
    method get_t_a    = cpSegmentShapeGetTA ~segment_shape
    method get_t_b    = cpSegmentShapeGetTB ~segment_shape
    method get_t_norm = cpSegmentShapeGetTNorm ~segment_shape
  end
;;

class cp_poly_shape ~shape =
  object
    val poly =
      let shape = shape#shape in
      (*
      if cpShapeGetType ~shape <> CP_POLY_SHAPE then raise Wrong_shape_kind;
      *)
      cpPolyShape_of_cpShape ~shape

    method get_num_verts = cpPolyShapeGetNumVerts ~poly
    method get_verts     = cpPolyShapeGetVects ~poly
    method get_t_verts   = cpPolyShapeGetTVerts ~poly
  end
;;


(* {{{ virtual cp_shape *)

class virtual cp_shape_virt (_shape:Low_level.cpShape) =
  object
    val shape = _shape

    method virtual body : cp_body_virt
    method virtual free : unit
    method virtual get_elasticity : float
    method virtual get_friction : float

    method virtual get_circle_shape : cp_circle_shape
    method virtual get_poly_shape : cp_poly_shape
    method virtual get_segment_shape : cp_segment_shape

    method virtual get_surface_vel : Low_level.cpVect
    (*
    method virtual is_circle_shape : bool
    method virtual is_poly_shape : bool
    method virtual is_segment_shape : bool
    method virtual kind : Low_level.cpShapeType
    *)
    method virtual set_elasticity : e:float -> unit
    method virtual set_friction : u:float -> unit
    method virtual set_surface_vel : surface_vel:Low_level.cpVect -> unit
    method virtual shape : Low_level.cpShape

    (*
    method virtual set_collision_type : collision_type:int -> unit
    method virtual get_collision_type : int
    *)

    method virtual set_layers : layers:int -> unit
    method virtual get_layers : int

    (*
    method virtual set_group : group:int -> unit
    method virtual get_group : int
    *)
  end
(* }}} *)


(** parameter for each shape kind *)
type shape_kind =
  | CIRCLE_SHAPE of  float * cpVect          (** radius, center *)
  | SEGMENT_SHAPE of cpVect * cpVect * float (** a, b, thickness *)
  | POLY_SHAPE of    cpVect array * cpVect   (** verts, offset *)



class cp_shape ~body:(_body :cp_body_virt) ~kind:_kind =
  let _shape =
    let body = _body#body in
    match _kind with
    | CIRCLE_SHAPE  (radius, offset) -> cpCircleShapeNew ~body ~radius ~offset
    | SEGMENT_SHAPE (a, b, r)        -> cpSegmentShapeNew ~body ~a ~b ~r
    | POLY_SHAPE    (verts, offset)  -> cpPolyShapeNew ~body ~verts ~offset
  in
  object (self)
  inherit cp_shape_virt _shape

    method free = cpShapeFree ~shape
    method shape = shape

    (*
    method kind = cpShapeGetType ~shape
    *)
    method body = _body

    method get_circle_shape  = new cp_circle_shape ~shape:self
    method get_segment_shape = new cp_segment_shape ~shape:self
    method get_poly_shape    = new cp_poly_shape ~shape:self

    (*
    method is_circle_shape  = (self#kind = CP_CIRCLE_SHAPE)
    method is_segment_shape = (self#kind = CP_SEGMENT_SHAPE)
    method is_poly_shape    = (self#kind = CP_POLY_SHAPE)
    *)

    method get_elasticity = cpShapeGetElasticity ~shape
    method set_elasticity = cpShapeSetElasticity ~shape
    (** Coefficient of restitution. (elasticity) *)

    method get_friction   = cpShapeGetFriction ~shape
    method set_friction   = cpShapeSetFriction ~shape
    (** Coefficient of friction. *)

    method set_surface_vel ~surface_vel = cpShapeSetSurfaceVelocity shape surface_vel
    method get_surface_vel  = cpShapeGetSurfaceVelocity ~shape
    (** Surface velocity used when solving for friction. *)

    (*
    method get_collision_type = cpShapeGetCollisionType ~shape
    method set_collision_type = cpShapeSetCollisionType ~shape
    (** User defined collision type for the shape. *)
    *)

    (*
    method get_group = cpShapeGetGroup ~shape
    method set_group = cpShapeSetGroup ~shape
    (** User defined collision group for the shape. *)
    *)

    method get_layers = cpShapeGetLayers ~shape
    method set_layers = cpShapeSetLayers ~shape
    (** User defined layer bitmask for the shape. *)
  end


let to_cp_shape ~shape =
  object (self)
  inherit cp_shape_virt shape

    method free = cpShapeFree ~shape
    method shape = shape

    (*
    method kind = cpShapeGetType ~shape
    *)
    method body = to_cp_body (cpShapeGetBody ~shape)

    method get_circle_shape  = new cp_circle_shape ~shape:self
    method get_segment_shape = new cp_segment_shape ~shape:self
    method get_poly_shape    = new cp_poly_shape ~shape:self

    (*
    method is_circle_shape  = (self#kind = CP_CIRCLE_SHAPE)
    method is_segment_shape = (self#kind = CP_SEGMENT_SHAPE)
    method is_poly_shape    = (self#kind = CP_POLY_SHAPE)
    *)

    method set_elasticity = cpShapeSetElasticity ~shape
    method set_friction   = cpShapeSetFriction ~shape
    method set_surface_vel ~surface_vel  = cpShapeSetSurfaceVelocity shape surface_vel

    method get_elasticity  = cpShapeGetElasticity ~shape
    method get_friction    = cpShapeGetFriction ~shape
    method get_surface_vel = cpShapeGetSurfaceVelocity ~shape

    (*
    method get_collision_type = cpShapeGetCollisionType ~shape
    method set_collision_type = cpShapeSetCollisionType ~shape
    *)

    (*
    method get_group = cpShapeGetGroup ~shape
    method set_group = cpShapeSetGroup ~shape
    *)

    method get_layers = cpShapeGetLayers ~shape
    method set_layers = cpShapeSetLayers ~shape
  end
;;

(* }}} *)
(* {{{ Space *)

class cp_space =
  object
    val space = cpSpaceNew()

    method free = cpSpaceFree ~space;

    method space = space

    method add_body ~(body:cp_body_virt) = cpSpaceAddBody ~space ~body:body#body

    method add_shape ~(shape:cp_shape_virt)    = cpSpaceAddShape ~space ~shape:shape#shape
    (** Add the given shape to the space's active spatial hash. Shapes attached to
        moving bodies should be added here as they will be rehashed on every call
        to [space#step]. *)

    method add_static_shape ~(shape:cp_shape_virt) = cpSpaceAddStaticShape ~space ~shape:shape#shape
    (** Add the given shape to the space's static spatial hash. *)

    method remove_shape ~(shape:cp_shape_virt) = cpSpaceRemoveShape ~space ~shape:shape#shape
    method remove_static_shape ~(shape:cp_shape_virt) = cpSpaceRemoveStaticShape ~space ~shape:shape#shape
    method remove_body ~(body:cp_body_virt) = cpSpaceRemoveBody ~space ~body:body#body

    method add_constraint ~(constr:cpConstraint) = cpSpaceAddConstraint ~space ~constr
    method remove_constraint ~(constr:cpConstraint) = cpSpaceRemoveConstraint ~space ~constr

    method step = cpSpaceStep ~space
    (** Move the space forward by dt seconds. Using a fixed size time step is
        {i highly} recommended for efficiency of the contact persistence algorithm. *)

    method get_iterations = cpSpaceGetIterations ~space
    method set_iterations = cpSpaceSetIterations ~space
    (** The number of iterations to use when solving constraints. (collisions and joints) *)

    method get_gravity    = cpSpaceGetGravity ~space
    method set_gravity    = cpSpaceSetGravity ~space
    (** The amount of gravity in the system. *)

    method get_damping    = cpSpaceGetDamping ~space
    method set_damping    = cpSpaceSetDamping ~space
    (** The amount of damping to apply to the system when updating. *)

    (**
      {b Notes:}

      When removing objects from the space, make sure you remove any other objects that
      reference it. For instance, when you remove a body, remove the joints and shapes
      attached to it.

      The number of iterations, and the size of the time step determine the quality of
      the simulation. More iterations, or smaller time steps increase the quality.
    *)
  end
;;

(* }}} *)

let damped_spring ~(a :cp_body_virt) ~(b :cp_body_virt) = cpDampedSpringNew ~a:a#body ~b:b#body ;;
(** Apply a spring force between bodies a and b at anchors anchr1 and anchr2
    respectively. k is the spring constant (force/distance), rlen is the rest
    length of the spring, dmp is the damping constant (force/velocity), and dt
    is the time step to apply the force over. Note: not solving the damping
    forces in the impulse solver causes problems with large damping values. This
    function will eventually be replaced by a new constraint (joint) type. *)
end
#endif


(* vim: sw=2 sts=2 ts=2 et filetype=ocaml fdm=marker
 *)
