/* {{{ COPYING 

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

}}} */

//define CP_USE_DEPRECATED_API_4 1
#include <chipmunk/chipmunk.h>
#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/callback.h>
#include <caml/fail.h>

#include "wrap_chipmunk.h"

// {{{ chipmunk.h 

CAMLprim value
ml_cpInitChipmunk( value unit )
{
    cpInitChipmunk();
    return Val_unit;
}


CAMLprim value
get_chipmunk_compile_version( value unit )
{
    CAMLparam0();
    CAMLlocal1( ml_version );
    ml_version = caml_alloc(3, 0);
    Store_field( ml_version, 0, Val_int(CP_VERSION_MAJOR) );
    Store_field( ml_version, 1, Val_int(CP_VERSION_MINOR) );
    Store_field( ml_version, 2, Val_int(CP_VERSION_RELEASE) );
    CAMLreturn( ml_version );
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


CAMLprim value
ml_cpAreaForPoly( value ml_verts )
{
    int i;
    int numVerts = Wosize_val(ml_verts);
    cpVect verts[numVerts];

    for(i=0; i<numVerts; i++) {
        verts[i].x = Double_field(Field(ml_verts,i),0);
        verts[i].y = Double_field(Field(ml_verts,i),1);
    }

    cpFloat r = cpAreaForPoly( numVerts, verts );
    return caml_copy_double(r);
}


// }}}
// {{{ cpBody.h 


CAMLprim value
ml_cpBodyNew( value m, value i )
{
    cpBody *body = cpBodyNew( Double_val(m), Double_val(i) );
    return Val_cpBody(body);
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

    printf("  angle: %g\n", body->a );
    printf("  angular velocity: %g\n", body->w );
    printf("  torque: %g\n", body->t );

    printf("  unit length rot: %g %g\n", body->rot.x, body->rot.y );
    printf("}\n");
    fflush(stdout);
}

CAMLprim value
ml_cpBodyDump( value body )
{
    dump_cpBody( cpBody_val(body) );
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
    printf("  idleSpeedThreshold: %g\n", space->idleSpeedThreshold );
    printf("  sleepTimeThreshold: %g\n", space->sleepTimeThreshold );
    printf("  collisionSlop: %g\n", space->collisionSlop );
    printf("  collisionBias: %g\n", space->collisionBias );
    printf("}\n");
    fflush(stdout);
}

CAMLprim value
ml_cpSpaceDump( value space )
{
    dump_cpSpace( cpSpace_val(space) );
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

    cpShape* shape = cpSegmentShapeNew( cpBody_val(body), a, b, Double_val(r) );

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

    cpShape *shape = cpPolyShapeNew( cpBody_val(body), numVerts, verts, offset );
    return (value) shape;
}


CAMLprim value
ml_cpCircleShapeNew( value body, value radius, value ml_offset )
{
    cpVect offset;
    offset.x = Double_field(ml_offset,0);
    offset.y = Double_field(ml_offset,1);

    cpShape *shape = cpCircleShapeNew( cpBody_val(body), Double_val(radius), offset );
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

/* TODO
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
*/

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
ml_cpBBIntersects( value a, value b )
{
    if (cpBBIntersects( *((cpBB *)a), *((cpBB *)b) )) return Val_true; else return Val_false;
}

CAMLprim value
ml_cpBBContainsBB( value bb, value other )
{
    if (cpBBContainsBB( *((cpBB *)bb), *((cpBB *)other) )) return Val_true; else return Val_false;
}

CAMLprim value
ml_cpBBContainsVect( value bb, value ml_v )
{
    cpVect v;
    v.x = Double_field(ml_v,0);
    v.y = Double_field(ml_v,1);

    if (cpBBContainsVect( *((cpBB *)bb), v )) return Val_true; else return Val_false;
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
    return Val_cpSpace(space);
}

// }}}
// {{{ cpArbiter.h 


CAMLprim value
ml_cpArbiterGetShapePA( value arbiter )
{
    cpShape *a, *b;
    cpArbiterGetShapes(((cpArbiter *) arbiter), &a, &b);
    return Val_cpShape(a);
}

CAMLprim value
ml_cpArbiterGetShapePB( value arbiter )
{
    cpShape *a, *b;
    cpArbiterGetShapes(((cpArbiter *) arbiter), &a, &b);
    return Val_cpShape(b);
}

CAMLprim value
ml_cpArbiterGetShapes( value arbiter )
{
    CAMLparam1( arbiter );
    CAMLlocal1( ml_shapes );

    cpShape *a, *b;
    cpArbiterGetShapes(((cpArbiter *) arbiter), &a, &b);

    ml_shapes = caml_alloc(2, 0);
    Store_field( ml_shapes, 0, (value) a );
    Store_field( ml_shapes, 1, (value) b );
    CAMLreturn( ml_shapes );
}

// }}}

CAMLprim value
Val_cpHashValue( cpHashValue v )
{
// in <chipmunk/chipmunk_types.h> we have
// typedef unsigned int cpHashValue;
    if (v >> 31) caml_failwith("cpHashValue"); // check overflow
    return Val_long(v);
}

#include "wrap_chipmunk.gen.c"

// vim: sw=4 sts=4 ts=4 et fdm=marker
