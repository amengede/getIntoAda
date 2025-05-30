with Ada.Text_IO; use Ada.Text_IO;
with Ada.Text_IO.Unbounded_IO; use Ada.Text_IO.Unbounded_IO;
with Interfaces.C.Extensions;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Vulkan.Extensions.EXT_Debug_Utils;

package body Logger is

    package Debug_Utils renames Vulkan.Extensions.EXT_Debug_Utils;
    
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


    function Debug_Callback (
        Message_Severity : Debug_Utils_Message_Severity_Flags;
        Message_Types : Debug_Utils_Message_Type_Flags;
        Callback_Data : Debug_Utils_Messenger_Callback_Data;
        User_Data : Interfaces.C.Extensions.void_ptr) 
        return Boolean is
    begin
        Put_Line ("Validation Layer: " & Callback_Data.Message);

        return False;
    end Debug_Callback;

    ---------------------------------------------------------------------------
    --  Make a debug messenger
    ---------------------------------------------------------------------------
    function Make_Debug_Messenger (Instance : Vulkan.Instance)
        return Debug_Utils_Messenger is

        Messenger_Info : Debug_Utils_Messenger_Create_Info;
        
    begin

        Debug_Utils.Load_Extension (Instance);

        Messenger_Info.Message_Severity :=
            Debug_Utils_Message_Severity_Error_Bit
            + Debug_Utils_Message_Severity_Warning_Bit;

        Messenger_Info.Message_Type :=
            Debug_Utils_Message_Type_General_Bit
            + Debug_Utils_Message_Type_Performance_Bit
            + Debug_Utils_Message_Type_Performance_Bit;

        Messenger_Info.User_Callback := Debug_Callback'Access;

        return Debug_Utils.Create (Instance, Messenger_Info);
    end Make_Debug_Messenger;

    
    procedure Destroy (
        Instance : in Vulkan.Instance;
        Debug_Messenger : in out Debug_Utils_Messenger) is
    begin
        Debug_Utils.Destroy (Instance, Debug_Messenger);
    end Destroy;

end Logger;
