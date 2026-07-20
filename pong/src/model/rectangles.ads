with SDL.Video.Palettes;
with SDL.Video.Rectangles;
with Constants;
with Vectors;
with Hit_Records; use Hit_Records;

package Rectangles is

    package Palettes renames SDL.Video.Palettes;

    type Rectangle is tagged private;
    --  @brief A general rectangle

    type Rectangle_Ptr is access all Rectangle;
    --  @brief Handle to a rectangle

    package Rectangle_Vectors is new Vectors (T => Rectangle_Ptr);
    --  @brief Collection of rectangle handles

    procedure Initialize (Self : in out Rectangle;
                          X, Y, Width, Height : Integer;
                          Colour : Palettes.Colour);
    --  @brief Initialize a new Rectangle
    --  @param Self Rectangle to initialize
    --  @param X X-Coordinate to set
    --  @param Y Y-Coordinate to set
    --  @param Width Width to set
    --  @param Height Height to set
    --  @param Colour Colour to set

    function Get_Colour (Self : Rectangle) return Palettes.Colour;
    --  @brief Get a rectangle's colour
    --  @param Self Rectangle to query
    --  @return The rectangle's colour as a tuple in (R, G, B, A) form

    function Get_Geometry (Self : Rectangle)
        return SDL.Video.Rectangles.Rectangle;
    --  @brief Get an SDL-Friendly representation of the Rectangle
    --  @param Self the Rectangle to query
    --  @return The data needed by SDL to draw the rectangle

    procedure Move_To (Self : in out Rectangle; X, Y : Integer);

    procedure Move_By (Self : in out Rectangle; Dx, Dy : Integer);
    --  @brief Move a rectangle by the given amount.
    --  @param Self Rectangle to move
    --  @param Dx: amount to move in x axis (pixels, +ve right, -ve left)
    --  @param Dy: amount to move in y axis (pixels, +ve down, -ve up)

    function Overlaps (Self, Other : Rectangle) return Boolean;
    --  @brief Do these rectangles overlap?
    --  @param Self First rectangle in test
    --  @param Other Second rectangle in test
    --  @return whether these two rectangles overlap with another rectangle

    function Move_And_Bounce (Self : in out Rectangle; Dx, Dy : Integer;
                              Restricted : Rectangle_Vectors.Vector)
        return Hit_Record;
    --  @brief Move the rectangle by the given amount, if it collides with
    --  any of the restricted rectangles, the rectangle will
    --  "bounce" off them.
    --  @param Self Rectangle to move
    --  @param Dx x component of movement
    --  @param Dy y component of movement
    --  @param restricted other rectangles to bounce off of
    --  @return Whether a bounce occurred in the x and y axis respectively

    procedure Move_And_Bounce (Self : Rectangle_Ptr; Dx, Dy : Integer;
                              Restricted : Rectangle_Vectors.Vector);
    --  @brief Move the rectangle by the given amount, if it collides with
    --  any of the restricted rectangles, the rectangle will
    --  "bounce" off them.
    --  @param Self Rectangle to move
    --  @param Dx x component of movement
    --  @param Dy y component of movement
    --  @param restricted other rectangles to bounce off of
private
    type Rectangle is tagged
        record
            X : Integer := 0;
            Y : Integer := 0;
            Width : Integer := 0;
            Height : Integer := 0;
            Colour : Palettes.Colour := Constants.WHITE;
        end record;
    --  @field X X-Coordinate of Top-Left corner
    --  @field Y Y-Coordinate of Top-Left corner
    --  @field Width Width of rectangle
    --  @field Height Height of rectangle
    --  @field Colour Colour of the rectangle
end Rectangles;
