#ifndef OCAML_CHIPMUNK_H
#define OCAML_CHIPMUNK_H


#define cpSpace_val(space)  ((cpSpace *) (space))
#define Val_cpSpace(space)  ((value) (space))

#define cpBody_val(body)  ((cpBody *) (body))
#define Val_cpBody(body)  ((value) (body))

#define cpShape_val(shape)  ((cpShape *) (shape))
#define Val_cpShape(shape)  ((value) (shape))
    
#define cpConstraint_val(constraint)  ((cpConstraint *) (constraint))
#define Val_cpConstraint(constraint)  ((value) (constraint))


#define Val_none Val_int(0)
#define Some_val(v) Field(v,0)

static inline value
Val_some(value v)
{   
    CAMLparam1(v);
    CAMLlocal1(some);
    some = caml_alloc(1, 0);
    Store_field(some, 0, v);
    CAMLreturn(some);
}

#endif /* OCAML_CHIPMUNK_H */
