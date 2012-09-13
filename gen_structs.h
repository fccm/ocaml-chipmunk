
cpBody body {
        Mass( float m ),
        MassInverse( float m_inv ),
        Moment( float i ),
        MomentInverse( float i_inv ),

        Pos( cpVect p ),
        Vel( cpVect v ),
        Force( cpVect f ),
        VelBias( cpVect v_bias ),

        Angle( float a ),
        AVel( float w ),
        Torque( float t ),
        AVelBias( float w_bias ),

        Rot( cpVect rot ),
};


cpSpace space {
        Iterations( int iterations ),

        Gravity( cpVect gravity ),
        Damping( float damping ),

        ElasticIterations( int elasticIterations ),

        Stamp( int_readonly stamp ),

//      Bodies( cpArray bodies ),
};


// Seems that the author would be willing to make
// this structure disappear in futur versions.
//
//cpArray cp_array {
//      Num( int num ),
//      Max( int max ),
//      void **arr;
//};


cpShape shape {
        Elasticity( float e ),
        Friction( float u ),
        SurfaceV( cpVect surface_v ),

// User defined collision type for the shape.
        CollisionType( int collision_type ),
// User defined collision group for the shape.
        Group( int group ),
// User defined layer bitmask for the shape.
        Layers( int layers ),

// Unique id used as the hash value.
        ID( int_readonly id ),
};


cpCircleShape circle_shape {
        Center( cpVect c ),
        Radius( float r ),
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


cpPinJoint pin_joint {
//      cpJoint joint;
        Anchor1( cpVect anchr1 ),
        Anchor2( cpVect anchr2 ),
        Dist( cpFloat dist),

        R1( cpVect r1 ),
        R2( cpVect r2 ),
        Vect( cpVect n ),
        NMass( cpFloat nMass ),

        JNAcc( cpFloat jnAcc ),
        JBias( cpFloat jBias ),
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
        JBias( cpFloat jBias ),
        Bias( cpFloat bias ),
};


cpPivotJoint pivot_joint {
//      cpJoint joint;
        Anchor1( cpVect anchr1 ),
        Anchor2( cpVect anchr2 ),

        R1( cpVect r1 ),
        R2( cpVect r2 ),

        K1( cpVect k1 ),
        K2( cpVect k2 ),

        JAcc( cpVect jAcc ),
        JBias( cpVect jBias ),
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

        K1( cpVect k1 ),
        K2( cpVect k2 ),

        JAcc( cpVect jAcc ),
        JBias( cpVect jBias ),
        Bias( cpVect bias ),
};


// Data structure for contact points.
cpContact cp_contact {
// Contact point and normal.
        P( cpVect p ),
        N( cpVect n ),
// Penetration distance.
        Dist( cpFloat dist ),

// Calculated by cpArbiterPreStep().
        R1( cpVect r1 ),
        R2( cpVect r2 ),
        NMass( cpFloat nMass ),
        TMass( cpFloat tMass ),
        Bounce( cpFloat bounce ),

// Persistant contact information.
        JNAcc( cpFloat jnAcc ),
        JTAcc( cpFloat jtAcc ),
        JBias( cpFloat jBias ),
        Bias( cpFloat bias ),

// Hash value used to (mostly) uniquely identify a contact.
//      unsigned int hash;
        Hash( int hash ),
};


// Data structure for tracking collisions between shapes.
cpArbiter arbiter {
// Information on the contact points between the objects.
        NumContacts( int numContacts ),
//      Contacts( cpContactArray contacts ),

// The two shapes involved in the collision.
        A( cpShape a ),
        B( cpShape b ),

// Calculated by cpArbiterPreStep().
        U( cpFloat u ),
//      E( cpFloat e ),
        TargetV( cpVect target_v ),

// Time stamp of the arbiter. (from cpSpace)
        Stamp( int stamp ),
};



cpBB bb {
        L( cpFloat l ),
        B( cpFloat b ),
        R( cpFloat r ),
        T( cpFloat t ),
} cpBB;


// vim: et
