program SetRFIDWiFi;

uses
  Forms,
  main in 'main.pas' {Mainform},
  uFCoder in 'uFCoder.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(Tmainform, mainform);
  Application.Run;
end.
