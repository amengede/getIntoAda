with GL.Types;
with GL.Objects.Buffers;
with GL.Objects.Vertex_Arrays;
with Interfaces.C.Pointers;

package Meshes is
    package Types renames GL.Types;
    package Buffers renames GL.Objects.Buffers;
    package Vertex_Arrays renames GL.Objects.Vertex_Arrays;

    type Mesh is record
        Buffer : Buffers.Buffer;
        Vertex_Array_Object : Vertex_Arrays.Vertex_Array_Object;
        Index_Buffer_Offset : Integer := 0;
        Index_Count : Types.Size := 0;
    end record;

    function Make_RGB_Triangle return Mesh;
    --  function Make_RGB_Quad return Mesh;

private
    type Vertex is record
        X, Y : Types.Single;
        Color_ID : Types.UByte;
        U, V : Types.Single;
    end record with Object_Size => 136,
    Alignment => 1;

    type Index_Data is array (Natural range <>) of aliased Types.UByte;
    type Vertex_Data is array (Natural range <>) of aliased Vertex;

    package Vertex_Pointers is
        new Interfaces.C.Pointers (
            Index => Natural,
            Element => Vertex,
            Element_Array => Vertex_Data,
            Default_Terminator => Vertex'(0.0, 0.0, 0, 0.0, 0.0));
    package Index_Pointers is
        new Interfaces.C.Pointers (
            Index => Natural,
            Element => Types.UByte,
            Element_Array => Index_Data,
            Default_Terminator => 0);
    procedure Upload is new Buffers.Set_Sub_Data (Vertex_Pointers);
    procedure Upload is new Buffers.Set_Sub_Data (Index_Pointers);
end Meshes;
