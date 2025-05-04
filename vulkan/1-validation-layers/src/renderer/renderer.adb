with Logger;
with Instances;

package body Renderer is

    procedure Initialize is
    begin
        Logger.Print ("Start making a graphics engine");
        Instances.Initialize (Instance);
        Debug_Messenger := Logger.Make_Debug_Messenger (Instance);
    end Initialize;

    procedure Finalize is
    begin
        --  Logger.Destroy (Instance, Debug_Messenger);
        Instances.Destroy (Instance);
        Logger.Print ("Goodbye see you!");
    end Finalize;

end Renderer;
