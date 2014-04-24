

cpCircleShape circle_shape {
        Radius( cpFloat r ),
        Center( cpVect c ),
};


cpSegmentShape segment_shape {
//      Shape( cpShape shape ),

        A( cpVect a ),
        B( cpVect b ),
        Norm( cpVect n ),

        Thickness( float r ),

        TA( cpVect ta ),
        TB( cpVect tb ),
        TNorm( cpVect tn ),
};


cpPolyShape poly {
//      Shape( cpShape shape ),
        
        NumVerts( int_readonly numVerts ),
        Vects( cpVectArray verts ),
//      cpPolyShapeAxis *axes;

        TVerts( cpVectArray tVerts ),
//      cpPolyShapeAxis *tAxes;
};



cpConstraint constr {
        MaxForce( cpFloat maxForce ),
        ErrorBias( cpFloat errorBias ),
        MaxBias( cpFloat maxBias ),
};



cpPinJoint pin_joint {
//      cpConstraint constraint;
        Anchor1( cpVect anchr1 ),
        Anchor2( cpVect anchr2 ),
        Dist( cpFloat dist),

        R1( cpVect r1 ),
        R2( cpVect r2 ),
        Vect( cpVect n ),
        NMass( cpFloat nMass ),

        JNAcc( cpFloat jnAcc ),
        Bias( cpFloat bias ),
};


cpSlideJoint slide_joint {
//      cpJoint joint;
        Anchor1( cpVect anchr1 ),
        Anchor2( cpVect anchr2 ),
        Min( cpFloat min ),
        Max( cpFloat max ),
        
        R1( cpVect r1 ),
        R2( cpVect r2 ),
        Vect( cpVect n ),
        NMass( cpFloat nMass ),
        
        JNAcc( cpFloat jnAcc ),
        Bias( cpFloat bias ),
};


cpPivotJoint pivot_joint {
//      cpJoint joint;
        Anchor1( cpVect anchr1 ),
        Anchor2( cpVect anchr2 ),

        R1( cpVect r1 ),
        R2( cpVect r2 ),

        JAcc( cpVect jAcc ),
        Bias( cpVect bias ),
};


cpGrooveJoint groove_joint {
//      cpJoint joint;
        GrvN( cpVect grv_n ),
        GrvA( cpVect grv_a ),
        GrvB( cpVect grv_b ),
        Anchor2( cpVect anchr2 ),

        GrvTn( cpVect grv_tn ),
        Clamp( cpFloat clamp ),

        R1( cpVect r1 ),
        R2( cpVect r2 ),

        JAcc( cpVect jAcc ),
        Bias( cpVect bias ),
};



// Data structure for tracking collisions between shapes.
cpArbiter arbiter {
// Information on the contact points between the objects.

// Calculated before calling the pre-solve collision handler
// Override them with custom values if you want specialized behavior
        E( cpFloat e ),
        U( cpFloat u ),
// Used for surface_v calculations, implementation may change
        Surface_vr( cpVect surface_vr),

//      struct cpCollisionHandler *handler;
};



cpBB bb {
        L( cpFloat l ),
        B( cpFloat b ),
        R( cpFloat r ),
        T( cpFloat t ),
} cpBB;


// vim: et
