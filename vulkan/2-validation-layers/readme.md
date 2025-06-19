After creating a Vulkan instance, we can configure a debug messenger. Vulkan allows very fine-grained validation checks on its API calls. Whenever we call a Vulkan function, a validation layer intercepts our call and validates it. If the configuration is wrong, or if an error occurs during execution, a message is logged to the terminal and the validation layer attempts to correct the error. The correction happens automatically in the background so long as validation layers are enabled, but unless we configure a debug messenger the error message will not appear.

Firstly, let's create a function to serve as our callback.

<src/logging/logger.adb>:
```
...
with Vulkan.Extensions.EXT_Debug_Utils;

package body Logger is

    package Debug_Utils renames Vulkan.Extensions.EXT_Debug_Utils;
    
    ...

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

    ...

end Logger;
```

Now let's declare and define a function to create and destroy the messenger.

<src/logging/logger.ads>:
```
...
with Vulkan.Extensions.EXT;
use Vulkan.Extensions.EXT;
-------------------------------------------------------------------------------
--  Logger: Prints out messages
-------------------------------------------------------------------------------
package Logger is

    ...

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

...
end Logger;
-------------------------------------------------------------------------------
```

<src/logging/logger.adb>:
```
...
package body Logger is

    ...

    function Make_Debug_Messenger (Instance : Vulkan.Instance)
        return Debug_Utils_Messenger is

        Messenger_Info : Debug_Utils_Messenger_Create_Info;
        
    begin

        Debug_Utils.Load_Extension (Instance);

        Messenger_Info.Message_Severity :=
            Debug_Utils_Message_Severity_Error_Bit
            + Debug_Utils_Message_Severity_Warning_Bit;

        Messenger_Info.Message_Type :=
            Debug_Utils_Message_Type_Performance_Bit
            + Debug_Utils_Message_Type_Validation_Bit;

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
```

And there we have it! We can manage the messenger from the Renderer package.

<src/renderer/renderer.ads>:
```
-------------------------------------------------------------------------------
--  Renderer: Draws stuff
-------------------------------------------------------------------------------
...
with Vulkan.Extensions.EXT;
use Vulkan.Extensions.EXT;

package Renderer is

    ...

private

    ...

    ---------------------------------------------------------------------------
    --  Debug Messenger
    ---------------------------------------------------------------------------
    Debug_Messenger : Debug_Utils_Messenger;
end Renderer;
-------------------------------------------------------------------------------
```

<src/renderer/renderer.adb>:
```
...

package body Renderer is

    procedure Initialize (Window : Windows.Window) is
        SDL_Extensions : 
            constant SDL_Vulkan.Extension_Name_Arrays
                := Get_SDL_Extensions (Window);
    begin
        ...
        Debug_Messenger := Logger.Make_Debug_Messenger (Instance);
    end Initialize;

    procedure Finalize is
    begin
        Logger.Destroy (Instance, Debug_Messenger);
        ...
    end Finalize;

end Renderer;
```

It may not look like it's doing much right now, but if we add Debug_Utils_Message_Type_General_Bit to the messengers message_type field it'll start giving us errors! Neat!