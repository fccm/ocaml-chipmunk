

// cpSpace.h

void cpSpaceDestroy(cpSpace *space);
void cpSpaceFree(cpSpace *space);
void cpSpaceFreeChildren(cpSpace *space);
void cpSpaceAddShape(cpSpace *space, cpShape *shape);
void cpSpaceAddStaticShape(cpSpace *space, cpShape *shape);
void cpSpaceAddBody(cpSpace *space, cpBody *body);
void cpSpaceAddConstraint(cpSpace *space, cpConstraint *constr);
void cpSpaceRemoveShape(cpSpace *space, cpShape *shape);
void cpSpaceRemoveStaticShape(cpSpace *space, cpShape *shape);
void cpSpaceRemoveBody(cpSpace *space, cpBody *body);
void cpSpaceRemoveConstraint(cpSpace *space, cpConstraint *constr);
void cpSpaceResizeStaticHash(cpSpace *space, cpFloat dim, int count);
void cpSpaceResizeActiveHash(cpSpace *space, cpFloat dim, int count);
void cpSpaceRehashStatic(cpSpace *space);
void cpSpaceStep(cpSpace *space, cpFloat dt);


// cpBody.h

void cpBodyFree(cpBody *body);
//void cpBodySetMass(cpBody *body, cpFloat m);
//void cpBodySetMoment(cpBody *body, cpFloat i);
//void cpBodySetAngle(cpBody *body, cpFloat a);
void cpBodySlew(cpBody *body, cpVect pos, cpFloat dt);
void cpBodyUpdateVelocity(cpBody *body, cpVect gravity, cpFloat damping, cpFloat dt);
void cpBodyUpdatePosition(cpBody *body, cpFloat dt);
cpVect cpBodyLocal2World(cpBody *body, cpVect v);
cpVect cpBodyWorld2Local(cpBody *body, cpVect v);
void cpBodyApplyImpulse(cpBody *body, cpVect j, cpVect r);
void cpBodyApplyBiasImpulse(cpBody *body, cpVect j, cpVect r);
void cpBodyResetForces(cpBody *body);
void cpBodyApplyForce(cpBody *body, cpVect f, cpVect r);


// cpShape.h

void cpShapeFree(cpShape *shape);
void cpResetShapeIdCounter(void);


// cpJoint.h

cpConstraint *cpPinJointNew(cpBody *a, cpBody *b, cpVect anchr1, cpVect anchr2);
cpConstraint *cpSlideJointNew(cpBody *a, cpBody *b, cpVect anchr1, cpVect anchr2, cpFloat min, cpFloat max);
cpConstraint *cpPivotJointNew(cpBody *a, cpBody *b, cpVect pivot);
cpConstraint *cpGrooveJointNew(cpBody *a, cpBody *b, cpVect groove_a, cpVect groove_b, cpVect anchr2);

// Constraints

void cpConstraintFree(cpConstraint *constr);
cpConstraint *cpDampedSpringNew(cpBody *a, cpBody *b, cpVect anchr1, cpVect anchr2, cpFloat rest_length, cpFloat stiffness, cpFloat damping);

