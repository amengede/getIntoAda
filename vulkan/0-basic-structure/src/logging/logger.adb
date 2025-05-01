with Ada.Text_IO; use Ada.Text_IO;

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

end Logger;
