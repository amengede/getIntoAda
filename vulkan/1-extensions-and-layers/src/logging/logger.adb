with Ada.Text_IO; use Ada.Text_IO;
with Ada.Text_IO.Unbounded_IO; use Ada.Text_IO.Unbounded_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package body Logger is

    procedure Set_Mode (Mode : Boolean) is
    begin
        Enabled := Mode;
    end Set_Mode;

    function Is_Enabled return Boolean is
    begin
        return Enabled;
    end Is_Enabled;

    procedure Print (Message : String) is
    begin
        if not Enabled then
            return;
        end if;

        Put_Line (Message);
    end Print;

    procedure Print (List : String_Vectors.Vector) is
    begin
        if not Enabled then
            return;
        end if;

        for Line of List loop
            Put_Line (Line);
        end loop;
    end Print;

    procedure Print (
        Extensions : Vulkan.Extension_Properties_Vectors.Vector) is
    begin
        if not Enabled then
            return;
        end if;

        for Extension of Extensions loop
            Put_Line (Extension.Name);
        end loop;
    end Print;

    procedure Print (Layers : Vulkan.Layer_Properties_Vectors.Vector) is
    begin
        if not Enabled then
            return;
        end if;

        for Layer of Layers loop
            Put_Line (Layer.Name);
        end loop;
    end Print;

end Logger;
