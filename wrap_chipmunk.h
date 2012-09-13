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


#endif /* OCAML_CHIPMUNK_H */
