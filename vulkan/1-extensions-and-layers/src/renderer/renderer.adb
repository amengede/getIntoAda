with Logger;
with Instances;
with SDL_Backend; use SDL_Backend;

package body Renderer is
    procedure Initialize (Window : Windows.Window) is
        SDL_Extensions : 
            constant SDL_Vulkan.Extension_Name_Arrays
                := Get_SDL_Extensions (Window);
    begin
        Logger.Print ("Start making a graphics  engine");
        Instances.Initialize (Instance, SDL_Extensions);
    end Initialize;

    procedure Finalize is
    begin
        Instances.Destroy (Instance);
        Logger.Print ("Goodbye see you!");
    end Finalize;
end Renderer;
