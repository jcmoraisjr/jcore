(*
  JCore, OPF Metadata Classes
  Copyright (C) 2014 Joao Morais

  See the file LICENSE.txt, included in this distribution,
  for details about the copyright.

  This library is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
*)

unit JCoreOPFMetadata;

{$I jcore.inc}

interface

uses
  JCoreMetadata,
  JCoreOPFADM;

type

  { TJCoreOPFAttrMetadata }

  TJCoreOPFAttrMetadata = class(TJCoreAttrMetadata)
  private
    FADMClass: TJCoreOPFADMClass;
  public
    function CreateADM(const AEntity: TObject): TJCoreOPFADM;
  end;

  { TJCoreOPFClassMetadata }

  TJCoreOPFClassMetadata = class(TJCoreClassMetadata)
  protected
    { TODO : Generics? }
    function InternalAttributeMetadataClass: TJCoreAttrMetadataClass; override;
    function IsReservedAttr(const AAttrName: ShortString): Boolean; override;
  public
    function AttributeByName(const AAttributeName: string): TJCoreOPFAttrMetadata;
  end;

  { TJCoreOPFModel }

  TJCoreOPFModel = class(TJCoreModel)
  protected
    { TODO : Generics? }
    function InternalMetadataClass: TJCoreClassMetadataClass; override;
  public
    function AcquireMetadata(const AClass: TClass): TJCoreOPFClassMetadata;
    class function AcquireModel: TJCoreOPFModel;
  end;

implementation

uses
  sysutils,
  JCoreOPFConsts;

{ TJCoreOPFAttrMetadata }

function TJCoreOPFAttrMetadata.CreateADM(const AEntity: TObject): TJCoreOPFADM;
begin
  if not Assigned(FADMClass) then
    FADMClass := TJCoreOPFADM.AcquireADMClass(PropInfo^.PropType);
  Result := FADMClass.Create(AEntity, PropInfo);
end;

{ TJCoreOPFClassMetadata }

function TJCoreOPFClassMetadata.InternalAttributeMetadataClass: TJCoreAttrMetadataClass;
begin
  Result := TJCoreOPFAttrMetadata;
end;

function TJCoreOPFClassMetadata.IsReservedAttr(const AAttrName: ShortString): Boolean;
begin
  Result := SameText(SPID, AAttrName) or inherited IsReservedAttr(AAttrName);
end;

function TJCoreOPFClassMetadata.AttributeByName(
  const AAttributeName: string): TJCoreOPFAttrMetadata;
begin
  Result := inherited AttributeByName(AAttributeName) as TJCoreOPFAttrMetadata;
end;

{ TJCoreOPFModel }

function TJCoreOPFModel.InternalMetadataClass: TJCoreClassMetadataClass;
begin
  Result := TJCoreOPFClassMetadata;
end;

function TJCoreOPFModel.AcquireMetadata(const AClass: TClass
  ): TJCoreOPFClassMetadata;
begin
  Result := inherited AcquireMetadata(AClass) as TJCoreOPFClassMetadata;
end;

class function TJCoreOPFModel.AcquireModel: TJCoreOPFModel;
begin
  Result := inherited AcquireModel as TJCoreOPFModel;
end;

end.

