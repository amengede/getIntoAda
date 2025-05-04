with Data_Structures;
with Vulkan; use Vulkan;
with Interfaces.C.Extensions;
-------------------------------------------------------------------------------
--  Logger: Prints out messages
-------------------------------------------------------------------------------
package Logger is

    ---------------------------------------------------------------------------
    --  Set the mode of the logger (enabled or disabled)
    ---------------------------------------------------------------------------
    procedure Set_Mode (Mode : Boolean);

    ---------------------------------------------------------------------------
    --  Returns whether the logger is currently enabled
    ---------------------------------------------------------------------------
    function Is_Enabled return Boolean;

    ---------------------------------------------------------------------------
    --  Attempt to print a message
    ---------------------------------------------------------------------------
    procedure Print (Message : String);

    ---------------------------------------------------------------------------
    --  Print a list of items
    ---------------------------------------------------------------------------
    procedure Print (List : String_Vectors.Vector);

    ---------------------------------------------------------------------------
    --  Print a list of Vulkan Extensions
    ---------------------------------------------------------------------------
    procedure Print (Extensions : Vulkan.Extension_Properties_Vectors.Vector);

    ---------------------------------------------------------------------------
    --  Print a list of Vulkan Layers
    ---------------------------------------------------------------------------
    procedure Print (Layers : Vulkan.Layer_Properties_Vectors.Vector);

    function Debug_Callback (
        Message_Severity : Vulkan.Debug_Utils_Message_Severity_Flags;
        Message_Types : Vulkan.Debug_Utils_Message_Type_Flags;
        Callback_Data : Vulkan.Debug_Utils_Messenger_Callback_Data;
        User_Data : Interfaces.C.Extensions.void_ptr) 
        return Boolean;
    ---------------------------------------------------------------------------
    --  Make a debug messenger
    ---------------------------------------------------------------------------
    function Make_Debug_Messenger (Instance : Vulkan.Instance)
        return Vulkan.Debug_Utils_Messenger;

    ---------------------------------------------------------------------------
    --  Destroy a debug messenger
    ---------------------------------------------------------------------------
    procedure Destroy (
        Instance : in Vulkan.Instance;
        Debug_Messenger: in out Vulkan.Debug_Utils_Messenger);

private
    Enabled : Boolean := True;
end Logger;
-------------------------------------------------------------------------------
