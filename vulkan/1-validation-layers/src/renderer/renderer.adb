with Logger;
with Instances;
with Vulkan.Debug_Utils;
with SDL_Backend; use SDL_Backend;

package body Renderer is

    procedure Initialize (Window : Windows.Window) is
        SDL_Extensions : 
            constant SDL_Vulkan.Extension_Name_Arrays
                := Get_SDL_Extensions (Window);
    begin
        Logger.Print ("Start making a graphics  engine");
        Instances.Initialize (Instance, SDL_Extensions);
        Vulkan.Debug_Utils.Load_Extension (Instance);
        Debug_Messenger := Logger.Make_Debug_Messenger (Instance);
    end Initialize;

    procedure Finalize is
    begin
        --  Logger.Destroy (Instance, Debug_Messenger);
        Instances.Destroy (Instance);
        Logger.Print ("Goodbye see you!");
    end Finalize;

end Renderer;
