with Vulkan; use Vulkan;
with Vulkan.Extensions.EXT;
use Vulkan.Extensions.EXT;
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

    ---------------------------------------------------------------------------
    --  Report a description of a physical device
    --  @param Device Physical device to report
    ---------------------------------------------------------------------------
    procedure Print (Device : Vulkan.Physical_Device);

    ---------------------------------------------------------------------------
    --  Make a debug messenger
    ---------------------------------------------------------------------------
    function Make_Debug_Messenger (Instance : Vulkan.Instance)
        return Debug_Utils_Messenger;

    ---------------------------------------------------------------------------
    --  Destroy a debug messenger
    ---------------------------------------------------------------------------
    procedure Destroy (
        Instance : in Vulkan.Instance;
        Debug_Messenger: in out Debug_Utils_Messenger);

private
    Enabled : Boolean := True;
end Logger;
-------------------------------------------------------------------------------
