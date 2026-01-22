With Interfaces; use Interfaces;
With Simd; Use Simd;

package Maths is

    type Flags is array (0 .. 3) of Boolean;

    -- A Three Dimensional Point
    type Point_3D is array (0 .. 2) of Integer_32;

    function Signed_Area (A, B, C : Int32x2) return Integer_32;

    -- A Triangle with 2D coordinates
    type Triangle_2D is array (0 .. 2) of Int32x2;

    -- Axis Aligned Bounding Box
    type AABB is array (0 .. 1) of Int32x2;

    -- Build an AABB which tightly encloses a Triangle
    function Enclose (Triangle : Triangle_2D) return AABB;

    -- Set of Corners used by Tiles
    type Box_Corners is array (0 .. 3) of Int32x2;

    -- Set of Barycentric Weights used by Tiles
    type Box_Weights is array (0 .. 3) of Point_3D;

    -- Tile used in Rasterization tasks
    type Rasterization_Tile is
        record
            Corner : Int32x2 := [0, 0];
            Extent : Int32x2 := [0, 0];
            Weights : Point_3D := [0, 0, 0];
        end record;

    -- Collection of Tiles resulting from a subdivision operation
    type Rasterization_Tile_Bundle is array (0 .. 3)
        of Rasterization_Tile;

    -- Possible overlap results between shapes
    type Overlap_Status is (Inside, Outside, Partial);

    -- Construct a Rasterization tile from an Axis Aligned Bounding Box
    function Make_Tile_From_AABB (Box: AABB;
                                  Weights : Point_3D)
        return Rasterization_Tile;

    -- Subdivide a rasterization tile into 2x2 subtiles
    function Subdivide_Tile (Tile: Rasterization_Tile;
                             X_Partials, Y_Partials: Point_3D)
        return Rasterization_Tile_Bundle;

    -- Does a tile contain a point?
    function Contains (Tile : Rasterization_Tile;
                       Pos : Int32x2) return Boolean;

    -- What is a Tile's status with regard to a given Triangle?
    function Get_Overlap_Status (Tile : Rasterization_Tile;
                                 Triangle : Triangle_2D;
                                 X_Partials, Y_Partials : Point_3D)
        return Overlap_Status;

    type Vec2 is array (0 .. 1) of IEEE_Float_32;
    type Vec3 is array (0 .. 2) of IEEE_Float_32;
    type Vec4 is array (0 .. 3) of IEEE_Float_32;

    type Vec3_Triplet is array (0 .. 2) of Vec3;

    type Vertex is
        record
            Position : Vec3;
            Color : Vec3;
        end record;

    type Attribute_Package is
        record
            Color : Vec3;
        end record;

    type Attribute_Bundle is
        record
            Colors : Vec3_Triplet;
        end record;

    type Vertex_Buffer is array (Natural range <>) of Vertex;

end Maths;
