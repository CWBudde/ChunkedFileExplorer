program ChunkedFileExplorer;

{$EXCESSPRECISION OFF}

uses
  FastMM4,
  Forms,
  CfeMain in 'CfeMain.pas' {FormChunkedFileExplorer},
  CfeChunkCommon in '..\Source\CfeChunkCommon.pas',
  CfeChunkPng in '..\Source\CfeChunkPng.pas',
  CfeChunkWave in '..\Source\CfeChunkWave.pas',
  CfeChunkAiff in '..\Source\CfeChunkAiff.pas',
  CfeChunkMp4 in '..\Source\CfeChunkMp4.pas',
  CfeUtils in '..\Source\CfeUtils.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormChunkedFileExplorer, FormChunkedFileExplorer);
  Application.Run;
end.
