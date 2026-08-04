#include "colors.inc"
#include "woods.inc"

global_settings { ambient_light <1,1,1> }

//camera { location <0, 0, -6> look_at  <0, 0,  0> }
//camera { location <5, 0, 0> look_at  <0, 0,  0> rotate -x*90}
camera { location <5, 1, -2.5> look_at  <0, 0,  0> sky <0,0,-10> } // rotate <-50,-10,100> }
light_source { <2, 5, -3> color White}

plane { z,1 texture { T_Wood25 scale 4 } }

//sphere { <0, 0, 0>, 0.5 texture { pigment { color Blue } } }
sphere { <-1, -1, 0>, 1 pigment { color Green } finish { specular 0.7 roughness 0.03 } }
sphere { <-1,  1, 0>, 1 pigment { color Green } finish { specular 0.7 roughness 0.03 } }
sphere { < 1,  1, 0>, 1 pigment { color Green } finish { specular 0.7 roughness 0.03 } }
sphere { < 1, -1, 0>, 1 pigment { color Red   } finish { specular 0.7 roughness 0.03 } }

box {  <  2,  2, 1 >, < -2  , -2  ,  1.1 > texture { T_Wood35 scale 4 } }
box {  <  2,  2, 1 >, < -2  ,  2.1, -0.3 > texture { T_Wood35 scale 4 } }
box {  < -2,  2, 1 >, < -2.1, -2  , -0.3 > texture { T_Wood35 scale 4 } }
box {  < -2, -2, 1 >, <  2  , -2.1, -0.3 > texture { T_Wood35 scale 4 } }
box {  <  2, -2, 1 >, <  2.1,  2  , -0.3 > texture { T_Wood35 scale 4 } }

