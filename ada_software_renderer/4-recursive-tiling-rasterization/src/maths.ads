package Maths is

    -- A Two Dimensional Point
    type Point_2D is array (0 .. 1) of Integer;

    -- A Three Dimensional Point
    type Point_3D is array (0 .. 2) of Integer;

    function Signed_Area (A, B, C : Point_2D) return Integer;

    -- A Triangle with 2D coordinates
    type Triangle_2D is array (0 .. 2) of Point_2D;

    -- Axis Aligned Bounding Box
    type AABB is array (0 .. 1) of Point_2D;

    -- Build an AABB which tightly encloses a Triangle
    function Enclose (Triangle : Triangle_2D) return AABB;

    -- Set of Corners used by Tiles
    type Box_Corners is array (0 .. 3) of Point_2D;

    -- Set of Barycentric Weights used by Tiles
    type Box_Weights is array (0 .. 3) of Point_3D;

    -- Set of Boolean flags used by Tiles
    type Box_Flags is array (0 .. 3) of Boolean;

    -- Tile used in Rasterization tasks
    -- @field Corners Positions of tile corners
    -- @field Weights Barycentric weights at each corner
    -- @Inside Flags indicating whether each corner is inside the
    -- Triangle
    type Rasterization_Tile is
        record
            Corners : Box_Corners := [[0, 0],
                                     [0, 0],
                                     [0, 0],
                                     [0, 0]];
            Weights : Box_Weights := [[0, 0, 0],
                                     [0, 0, 0],
                                     [0, 0, 0],
                                     [0, 0, 0]];
            Inside : Box_Flags := [False, False, False, False];
            Inside_Count : Integer := 0;
        end record;

    -- Collection of Tiles resulting from a subdivision operation
    type Rasterization_Tile_Bundle is array (0 .. 3)
        of Rasterization_Tile;

    -- Possible overlap results between shapes
    type Overlap_Status is (Inside, Outside, Partial);

    -- Construct a Rasterization tile from an Axis Aligned Bounding Box
    function Make_Tile_From_AABB (Box: AABB;
                                  Weights, X_Partials, Y_Partials: Point_3D)
        return Rasterization_Tile;

    -- Subdivide a rasterization tile into 2x2 subtiles
    function Subdivide_Tile (Tile: Rasterization_Tile;
                             X_Partials, Y_Partials: Point_3D)
        return Rasterization_Tile_Bundle;

    -- Does a tile contain a point?
    function Contains (Tile : Rasterization_Tile;
                       Pos : Point_2D) return Boolean;

    -- What is a Tile's status with regard to a given Triangle?
    function Get_Overlap_Status (Tile : Rasterization_Tile;
                                 Triangle : Triangle_2D)
        return Overlap_Status;

private

    -- Collection of 9 corners resulting from a tile split.
    type Corner_Bundle is array (0 .. 8) of Point_2D;

    -- Subdivide the corners of a Tile
    function Subdivide_Corners (Tile : Rasterization_Tile;
                                Half_Extent : Point_2D)
        return Corner_Bundle;

    -- Collection of 9 Barycentric Weights resulting from a tile split
    type Weight_Bundle is array (0 .. 8) of Point_3D;

    -- Collection of 9 Flags resulting from a tile split
    type Flag_Bundle is array (0 .. 8) of Boolean;

    -- Split a tile's weights
    function Subdivide_Weights (Tile : Rasterization_Tile;
                                Flags : in out Flag_Bundle;
                                X_Partials, Y_Partials : Point_3D;
                                Half_Extent : Point_2D)
        return Weight_Bundle;

    -- Evaluate the corner flags of a tile for later overlap test
    -- @param Tile Tile to test
    procedure Evaluate_Flags (Tile : in out Rasterization_Tile);

end Maths;
