/* {{{ COPYING 

  +-----------------------------------------------------------------------+
  |  This file is part of a binding for OCaml to the Chipmunk library.    |
  +-----------------------------------------------------------------------+
  |  Copyright (C) 2008  Florent Monnier  <fmonnier@linux-nantes.org>     |
  +-----------------------------------------------------------------------+
  |  This program is free software: you can redistribute it and/or        |
  |  modify it under the terms of the GNU General Public License          |
  |  as published by the Free Software Foundation, either version 3       |
  |  of the License, or (at your option) any later version.               |
  |                                                                       |
  |  This program is distributed in the hope that it will be useful,      |
  |  but WITHOUT ANY WARRANTY; without even the implied warranty of       |
  |  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the        |
  |  GNU General Public License for more details.                         |
  |                                                                       |
  |  You should have received a copy of the GNU General Public License    |
  |  along with this program.  If not, see <http://www.gnu.org/licenses/> |
  +-----------------------------------------------------------------------+

}}} */

//include "chipmunk.h"
#include <chipmunk/chipmunk.h>
#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/callback.h>
#include <caml/fail.h>

// {{{ chipmunk.h 

CAMLprim value
ml_cpInitChipmunk( value unit )
{
    cpInitChipmunk();
    return Val_unit;
}


CAMLprim value
ml_set_cp_bias_coef( value ml_cp_bias_coef )
{
    cp_bias_coef = Double_val(ml_cp_bias_coef);
    return Val_unit;
}

CAMLprim value
ml_get_cp_bias_coef( value unit )
{
    return caml_copy_double(cp_bias_coef);
}

CAMLprim value
ml_set_cp_collision_slop( value ml_cp_collision_slop )
{
    cp_collision_slop = Double_val(ml_cp_collision_slop);
    return Val_unit;
}

CAMLprim value
ml_get_cp_collision_slop( value unit )
{
    return caml_copy_double(cp_collision_slop);
}

CAMLprim value
ml_set_cp_joint_bias_coef( value ml_cp_joint_bias_coef )
{
    cp_joint_bias_coef = Double_val(ml_cp_joint_bias_coef);
    return Val_unit;
}

CAMLprim value
ml_get_cp_joint_bias_coef( value unit )
{
    return caml_copy_double(cp_joint_bias_coef);
}

CAMLprim value
ml_set_cp_contact_persistence( value ml_cp_contact_persistence )
{
    cp_contact_persistence = Int_val(ml_cp_contact_persistence);
    return Val_unit;
}

CAMLprim value
ml_get_cp_contact_persistence( value unit )
{
    return Val_int(cp_contact_persistence);
}


CAMLprim value
ml_cpMomentForCircle( value m, value r1, value r2, value ml_offset )
{
    cpVect offset;
    offset.x = Double_field(ml_offset,0);
    offset.y = Double_field(ml_offset,1);
    cpFloat i = cpMomentForCircle( Double_val(m), Double_val(r1), Double_val(r2), offset );
    return caml_copy_double(i);
}


CAMLprim value
ml_cpMomentForPoly( value m, value ml_verts, value ml_offset )
{
    int i;
    int numVerts = Wosize_val(ml_verts);
    cpVect verts[numVerts];
    cpVect offset;

    for(i=0; i<numVerts; i++) {
        verts[i].x = Double_field(Field(ml_verts,i),0);
        verts[i].y = Double_field(Field(ml_verts,i),1);
    }

    offset.x = Double_field(ml_offset,0);
    offset.y = Double_field(ml_offset,1);

    cpFloat inertia = cpMomentForPoly( Double_val(m), numVerts, verts, offset );
    return caml_copy_double(inertia);
}

// }}}
// {{{ cpBody.h 


CAMLprim value
ml_cpBodyNew( value m, value i )
{
    cpBody *body = cpBodyNew( Double_val(m), Double_val(i) );
    return (value)body;
}

// }}}
// {{{ DEBUG 

/* DEBUG */
#include <stdio.h>


void dump_cpBody( cpBody *body )
{
    printf("cpBody {\n");

    printf("  mass: %g\n", body->m );
    printf("  mass inverse: %g\n", body->m_inv );
    printf("  moment of inertia: %g\n", body->i );
    printf("  moment of inertia inverse: %g\n", body->i_inv );

    printf("  position: %g %g\n", body->p.x, body->p.y );
    printf("  velocity: %g %g\n", body->v.x, body->v.y );
    printf("  force: %g %g\n", body->f.x, body->f.y );
    printf("  vel bias: %g %g\n", body->v_bias.x, body->v_bias.y );

    printf("  angle: %g\n", body->a );
    printf("  angular velocity: %g\n", body->w );
    printf("  torque: %g\n", body->t );
    printf("  angular vel bias: %g\n", body->w_bias );

    printf("  unit length rot: %g %g\n", body->rot.x, body->rot.y );
    printf("}\n");
    fflush(stdout);
}

CAMLprim value
ml_cpBodyDump( value body )
{
    dump_cpBody( (cpBody *)body );
    return Val_unit;
}


char * get_cpShape_type( cpShapeType type )
{
    char *types[] = {
        "CP_CIRCLE_SHAPE",
        "CP_SEGMENT_SHAPE",
        "CP_POLY_SHAPE",
        "CP_NUM_SHAPES",
    };
    int i=0;  // wrong default
    switch (type)
    {
        case CP_CIRCLE_SHAPE:  i = 0; break;
        case CP_SEGMENT_SHAPE: i = 1; break;
        case CP_POLY_SHAPE:    i = 2; break;
        case CP_NUM_SHAPES:    i = 3; break;
    }
    return types[i];
}

void dump_cpSpace( cpSpace *space )
{
    printf("cpSpace {\n");
    printf("  iterations: %d\n", space->iterations );
    printf("  gravity: x, y = %g %g\n", space->gravity.x, space->gravity.y );
    printf("  damping: %g\n", space->damping );
    printf("  stamp: %d\n", space->stamp );
    printf("}\n");
    fflush(stdout);
}

CAMLprim value
ml_cpSpaceDump( value space )
{
    dump_cpSpace( (cpSpace *)space );
    return Val_unit;
}

// }}}
// {{{ cpShape.h 


CAMLprim value
ml_cpSegmentShapeNew( value body, value ml_a, value ml_b, value r )
{
    CAMLparam4(body, ml_a, ml_b, r);
    cpVect a, b;

    a.x = Double_field(ml_a,0);
    a.y = Double_field(ml_a,1);
    b.x = Double_field(ml_b,0);
    b.y = Double_field(ml_b,1);

    cpShape* shape = cpSegmentShapeNew( (cpBody *)body, a, b, Double_val(r) );

    CAMLreturn( (value)shape );
}


CAMLprim value
ml_cpPolyShapeNew( value body, value ml_verts, value ml_offset )
{
    int i;
    int numVerts = Wosize_val(ml_verts);
    cpVect verts[numVerts];
    cpVect offset;

    for(i=0; i<numVerts; i++) {
        verts[i].x = Double_field(Field(ml_verts,i),0);
        verts[i].y = Double_field(Field(ml_verts,i),1);
    }

    offset.x = Double_field(ml_offset,0);
    offset.y = Double_field(ml_offset,1);

    cpShape *shape = cpPolyShapeNew( (cpBody *)body, numVerts, verts, offset );
    return (value) shape;
}


CAMLprim value
ml_cpCircleShapeNew( value body, value radius, value ml_offset )
{
    cpVect offset;
    offset.x = Double_field(ml_offset,0);
    offset.y = Double_field(ml_offset,1);

    cpShape *shape = cpCircleShapeNew( (cpBody *)body, Double_val(radius), offset );
    return (value) shape;
}


CAMLprim value
ml_cpShapeGetBody(value shape)
{
    CAMLparam1(shape);

    cpShape * shape_p;
    cpBody * body_p;

    shape_p = (cpShape *) shape;

    body_p = shape_p->body;

    if (body_p == NULL)
        caml_failwith("No Body set");

    CAMLreturn( (value) body_p );
}


CAMLprim value
ml_cpShapeSetBody(value shape, value body)
{
        ((cpShape *)shape)->body = (cpBody *) body;
        return Val_unit;
}

static const cpShapeType shape_type_table[] = {
        CP_CIRCLE_SHAPE,
        CP_SEGMENT_SHAPE,
        CP_POLY_SHAPE,
        CP_NUM_SHAPES
};

CAMLprim value
ml_cpShapeGetType(value shape)
{
    int i=0;  // wrong default
    switch ((((cpShape *)shape)->klass)->type)
    {
        case CP_CIRCLE_SHAPE:  i = 0; break;
        case CP_SEGMENT_SHAPE: i = 1; break;
        case CP_POLY_SHAPE:    i = 2; break;
        case CP_NUM_SHAPES:    i = 3; break;
    }
    return Val_int(i);
}

CAMLprim value
ml_cpShapeGetBB(value shape)
{
    return (value) & ((cpShape *)shape)->bb;
}

// }}}
// {{{ cpBB.h 


CAMLprim value
ml_cpBBNew( value l, value b, value r, value t )
{
        cpBB *bb;
        bb = malloc(sizeof(cpBB));
        bb->l = Double_val(l);
        bb->b = Double_val(b);
        bb->r = Double_val(r);
        bb->t = Double_val(t);
        return (value)bb;
}

CAMLprim value
ml_cpBBFree( value ml_bb )
{
    cpBB *bb;
    bb = (cpBB *)ml_bb;
    free(bb);
    return Val_unit;
}


CAMLprim value
ml_cpBBintersects( cpBB * a, cpBB * b )
{
    if (a->l<=b->r && b->l<=a->r && a->b<=b->t && b->b<=a->t)
        return Val_true;
    else
        return Val_false;
}
/*
CAMLprim value
ml_cpBBintersects( value a, value b )
{
    if (cpBBintersects( *((cpBB *)a), *((cpBB *)b) )) return Val_true; else return Val_false;
}
*/

CAMLprim value
ml_cpBBcontainsBB( cpBB * bb, cpBB * other )
{
    //if (cpBBcontainsBB( *((cpBB *)bb), *((cpBB *)other) )) return Val_true; else return Val_false;
    if (bb->l < other->l && bb->r > other->r && bb->b < other->b && bb->t > other->t)
        return Val_true;
    else
        return Val_false;
}

CAMLprim value
ml_cpBBcontainsVect( value bb, value ml_v )
{
    cpVect v;
    v.x = Double_field(ml_v,0);
    v.y = Double_field(ml_v,1);

    if (cpBBcontainsVect( *((cpBB *)bb), v )) return Val_true; else return Val_false;
}

CAMLprim value
ml_cpBBClampVect( value bb, value ml_v )
{
    CAMLparam2( bb, ml_v );
    CAMLlocal1( ml_ret );

    cpVect v;
    v.x = Double_field(ml_v,0);
    v.y = Double_field(ml_v,1);

    cpVect _ret = cpBBClampVect( *((cpBB *)bb), v);

    ml_ret = caml_alloc(2 * Double_wosize, Double_array_tag);
    Store_double_field( ml_ret, 0, _ret.x );
    Store_double_field( ml_ret, 1, _ret.y );

    CAMLreturn( ml_ret );
}

CAMLprim value
ml_cpBBWrapVect( value bb, value ml_v )
{
    CAMLparam0();
    CAMLlocal1( ml_ret );

    cpVect v;
    v.x = Double_field(ml_v,0);
    v.y = Double_field(ml_v,1);

    cpVect _ret = cpBBWrapVect( *((cpBB *)bb), v);

    ml_ret = caml_alloc(2 * Double_wosize, Double_array_tag);
    Store_double_field( ml_ret, 0, _ret.x );
    Store_double_field( ml_ret, 1, _ret.y );

    CAMLreturn( ml_ret );
}

// }}}
// {{{ cpSpace.h 


CAMLprim value
ml_cpSpaceNew( value unit )
{
    cpSpace* space = cpSpaceNew();
    return (value)space;
}

// }}}
// {{{ cpArbiter.h 


CAMLprim value
ml_cpSpaceGetArbiters( value space )
{
    CAMLparam1( space );
    CAMLlocal1( ml_array );
    int i;

    cpArray *arr = ((cpSpace *)space)->arbiters;

    ml_array = caml_alloc(arr->num, 0);

    for(i=0; i < arr->num; i++)
    {
        // (cpArbiter *) arr->arr[i];

        Store_field( ml_array, i, (value) arr->arr[i] );
    }

    CAMLreturn( ml_array );
}

/*
CAMLprim value
ml_cpArbiterGetShapeA( value arbiter )
{
    cpArbiter * arb;
    cpShape *_a;
    arb = (cpArbiter *) arbiter;

    if (arb->a == NULL) {
        caml_failwith("Arbiter has no shape a");
    }

    _a = arb->a;

    return (value) _a;
}


CAMLprim value
ml_cpArbiterGetShapeB( value arbiter )
{
    cpArbiter * arb;
    cpShape *_b;
    arb = (cpArbiter *) arbiter;

    if (arb->b == NULL) {
        caml_failwith("Arbiter has no shape b");
    }

    _b = arb->b;

    return (value) _b;
}
*/

CAMLprim value
ml_cpArbiterGetShapeA( value arbiter )
{
    return (value) (((cpArbiter *) arbiter)->a);
}

CAMLprim value
ml_cpArbiterGetShapeB( value arbiter )
{
    return (value) (((cpArbiter *) arbiter)->b);
}


CAMLprim value
ml_cpArbiterGetContacts( value arbiter )
{
    CAMLparam1( arbiter );
    CAMLlocal1( ml_array );
    int i, num_c;

    cpArbiter *arb = (cpArbiter *) arbiter;
    num_c = arb->numContacts;

    ml_array = caml_alloc(num_c, 0);

    for(i=0; i < num_c; i++)
    {
        // (cpContact *) arb->contacts[i]

        Store_field( ml_array, i, (value) &(arb->contacts[i]) );
    }

    CAMLreturn( ml_array );
}

// }}}

#include "wrap_chipmunk.gen.c"

// vim: sw=4 sts=4 ts=4 et fdm=marker
