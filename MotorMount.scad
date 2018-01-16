cube_size = 52;

innerSize = 42; 

difference(){
union(){
difference(){

    union(){difference(){
    {difference(){
       
        {difference(){
        cube([cube_size, cube_size, cube_size], center = [0,0,0]);
            
        translate([5, 5, 5]) cube([innerSize, innerSize, innerSize + 5], center = [50,10,20]);
        }}
        
        translate([5, 20, 15]) cube([innerSize, innerSize, innerSize + 5], center = [50,10,20]);
    } 

    color("red") translate([-2, cube_size, 15]) rotate([45, 0,0,])  cube([cube_size+4, cube_size+5, cube_size*2], center = false);
    }
    }


    


    //rotate([90, 0,0])  translate([0,cube_size,-5]) cube([52, 30, 5]);
    rotate([90, 0,0])  translate([cube_size/2,cube_size,-4.9]) cylinder(4.9, 52/2, 52/2);
    rotate([90, 0,0])  translate([cube_size/2,0,-4.9]) cylinder(4.9, 52/2, 52/2);
    }



{rotate([90, 0,0])  translate([cube_size / 2,-15,-10]) cylinder(20, 4,4);}
{rotate([90, 0,0])  translate([cube_size / 2,cube_size + 15,-10]) cylinder(20, 4,4);}
}}

color("gray") translate([5, 15, 5]) cube([innerSize, innerSize, innerSize], center = [50,10,20]);
}

color("gray") rotate([0,90,0]) translate([-30, 0, -15]) cube([innerSize, 5, 10], center = false);