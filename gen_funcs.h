
// chipmunk.h

cpFloat cpAreaForCircle(cpFloat r1, cpFloat r2);
cpFloat cpMomentForSegment(cpFloat m, cpVect a, cpVect b);
cpFloat cpAreaForSegment(cpVect a, cpVect b, cpFloat r);
cpFloat cpMomentForBox(cpFloat m, cpFloat width, cpFloat height);


// cpSpace.h

void cpSpaceDestroy(cpSpace *space);
void cpSpaceFree(cpSpace *space);
void cpSpaceAddShape(cpSpace *space, cpShape *shape);
void cpSpaceAddStaticShape(cpSpace *space, cpShape *shape);
void cpSpaceAddBody(cpSpace *space, cpBody *body);
void cpSpaceAddConstraint(cpSpace *space, cpConstraint *constr);
void cpSpaceRemoveShape(cpSpace *space, cpShape *shape);
void cpSpaceRemoveStaticShape(cpSpace *space, cpShape *shape);
void cpSpaceRemoveBody(cpSpace *space, cpBody *body);
void cpSpaceRemoveConstraint(cpSpace *space, cpConstraint *constr);
void cpSpaceStep(cpSpace *space, cpFloat dt);
cpBool cpSpaceIsLocked(cpSpace *space);
cpBool cpSpaceContainsShape(cpSpace *space, cpShape *shape);
cpBool cpSpaceContainsBody(cpSpace *space, cpBody *body);
cpBool cpSpaceContainsConstraint(cpSpace *space, cpConstraint *constr);
void cpSpaceReindexStatic(cpSpace *space);
void cpSpaceReindexShape(cpSpace *space, cpShape *shape);
void cpSpaceReindexShapesForBody(cpSpace *space, cpBody *body);
void cpSpaceUseSpatialHash(cpSpace *space, cpFloat dim, int count);

// cpSpace.h struct getters/setters:
int cpSpaceGetIterations(cpSpace *space);
void cpSpaceSetIterations(cpSpace *space, int iterations);
cpVect cpSpaceGetGravity(cpSpace *space);
void cpSpaceSetGravity(cpSpace *space, cpVect gravity);
cpFloat cpSpaceGetDamping(cpSpace *space);
void cpSpaceSetDamping(cpSpace *space, cpFloat damping);
cpFloat cpSpaceGetIdleSpeedThreshold(cpSpace *space);
void cpSpaceSetIdleSpeedThreshold(cpSpace *space, cpFloat v);
cpFloat cpSpaceGetSleepTimeThreshold(cpSpace *space);
void cpSpaceSetSleepTimeThreshold(cpSpace *space, cpFloat v);
cpFloat cpSpaceGetCollisionSlop(cpSpace *space);
void cpSpaceSetCollisionSlop(cpSpace *space, cpFloat v);
cpFloat cpSpaceGetCollisionBias(cpSpace *space);
void cpSpaceSetCollisionBias(cpSpace *space, cpFloat v);
// cpTimestamp cpSpaceGetCollisionPersistence(cpSpace *space);
// void cpSpaceSetCollisionPersistence(cpSpace *space, cpTimestamp value);
cpBool cpSpaceGetEnableContactGraph(cpSpace *space);
void cpSpaceSetEnableContactGraph(cpSpace *space, cpBool v);
// cpDataPointer cpSpaceGetUserData(cpSpace *space);
// void cpSpaceSetUserData(cpSpace *space, cpDataPointer value);
// cpBody* cpSpaceGetStaticBody(cpSpace *space);
cpFloat cpSpaceGetCurrentTimeStep(cpSpace *space);


// cpBody.h

void cpBodyFree(cpBody *body);
void cpBodyUpdateVelocity(cpBody *body, cpVect gravity, cpFloat damping, cpFloat dt);
void cpBodyUpdatePosition(cpBody *body, cpFloat dt);
cpVect cpBodyLocal2World(cpBody *body, cpVect v);
cpVect cpBodyWorld2Local(cpBody *body, cpVect v);
void cpBodyApplyImpulse(cpBody *body, cpVect j, cpVect r);
void cpBodyResetForces(cpBody *body);
void cpBodyApplyForce(cpBody *body, cpVect f, cpVect r);
void cpBodyActivate(cpBody *body);
void cpBodyActivateStatic(cpBody *body, cpShape *filter);
void cpBodySleep(cpBody *body);
cpBool cpBodyIsSleeping(cpBody *body);
cpBool cpBodyIsStatic(cpBody *body);
cpBool cpBodyIsRogue(cpBody *body);
cpVect cpBodyGetVelAtWorldPoint(cpBody *body, cpVect point);
cpVect cpBodyGetVelAtLocalPoint(cpBody *body, cpVect point);
cpFloat cpBodyKineticEnergy(cpBody *body);

// cpBody.h struct getters/setters:
cpFloat cpBodyGetMass(cpBody *body);
void cpBodySetMass(cpBody *body, cpFloat m);
cpFloat cpBodyGetMoment(cpBody *body);
void cpBodySetMoment(cpBody *body, cpFloat i);
cpVect cpBodyGetPos(cpBody *body);
void cpBodySetPos(cpBody *body, cpVect p);
cpVect cpBodyGetVel(cpBody *body);
void cpBodySetVel(cpBody *body, cpVect v);
cpVect cpBodyGetForce(cpBody *body);
void cpBodySetForce(cpBody *body, cpVect f);
cpFloat cpBodyGetAngle(cpBody *body);
void cpBodySetAngle(cpBody *body, cpFloat a);
cpFloat cpBodyGetAngVel(cpBody *body);
void cpBodySetAngVel(cpBody *body, cpFloat w);
cpFloat cpBodyGetTorque(cpBody *body);
void cpBodySetTorque(cpBody *body, cpFloat t);
cpVect cpBodyGetRot(cpBody *body);
cpFloat cpBodyGetVelLimit(cpBody *body);
void cpBodySetVelLimit(cpBody *body, cpFloat v_limit);
cpFloat cpBodyGetAngVelLimit(cpBody *body);
void cpBodySetAngVelLimit(cpBody *body, cpFloat w_limit);
// cpDataPointer cpBodyGetUserData(cpBody *body);
// void cpBodySetUserData(cpBody *body, cpDataPointer value);


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

