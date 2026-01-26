with GL.Buffers;
with GL.Context;
with Ada.Text_IO; use Ada.Text_IO;

package body Renderer is

    procedure Initialize is
    begin
        Put_Line (GL.Context.Version_String);
        GL.Buffers.Set_Color_Clear_Value ([0.5, 0.0, 0.25, 1.0]);
    end Initialize;

    procedure Clear_Screen is
        Flags : constant GL.Buffers.Buffer_Bits :=
            GL.Buffers.Buffer_Bits'(Depth => False,
                                    Accum => False,
                                    Stencil => False,
                                    Color => True);
    begin
        GL.Buffers.Clear (Flags);
    end Clear_Screen;
end Renderer;
