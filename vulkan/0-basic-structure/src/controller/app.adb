with Logger;
with Renderer;
with SDL.Events.Events;

package body App is

    package Events renames SDL.Events.Events;
    use type SDL.Events.Event_Types;

    procedure Initialize (
        Width : SDL.Positive_Dimension;
        Height : SDL.Positive_Dimension) is
    begin
        Logger.Print ("Making App");
        SDL_Backend.Build_Window (Window, Width, Height, "Test");
        Logger.Print ("Made Window");
        Renderer.Initialize;
    end Initialize;

    procedure Run is
        Event : Events.Events;
        Running : Boolean := True;
    begin
        while Running loop

            while Events.Poll (Event) loop
                if Event.Common.Event_Type = SDL.Events.Quit then
                    Running := False;
                end if;
            end loop;

        end loop;
    end Run;

    procedure Finalize is
    begin
        Renderer.Finalize;
    end Finalize;

end App;
