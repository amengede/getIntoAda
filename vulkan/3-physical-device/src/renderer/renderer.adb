with Logger;
with Instances;
with SDL_Backend; use SDL_Backend;
with Devices;

package body Renderer is

    procedure Initialize (Window : Windows.Window) is
        SDL_Extensions : 
            constant SDL_Vulkan.Extension_Name_Arrays
                := Get_SDL_Extensions (Window);
    begin
        Logger.Print ("Start making a graphics  engine");
        Instances.Initialize (Instance, SDL_Extensions);
        Debug_Messenger := Logger.Make_Debug_Messenger (Instance);
        Devices.Choose_Physical_Device (Physical_Device, Instance);
    end Initialize;

    procedure Finalize is
    begin
        Logger.Destroy (Instance, Debug_Messenger);
        Instances.Destroy (Instance);
        Logger.Print ("Goodbye see you!");
    end Finalize;

end Renderer;
