with Logger;
with Instances;

package body Renderer is

    procedure Initialize is
    begin
        Logger.Print ("Start making a graphics engine");
        Instances.Initialize (Instance);
    end Initialize;

    procedure Finalize is
    begin
        Logger.Print ("Goodbye see you!");
    end Finalize;

end Renderer;
