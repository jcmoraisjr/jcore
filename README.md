# JCore

Some classes and frameworks I have implemented so far.

* OPF
* Dependency Injection Container
* Expression Parser

## OPF

Features being implemented:

* Entities inherits from TObject and declares native data types
* Support of any data type, registering new data mediators for unsupported PTypeInfos
* Auto and manual mapping (object <-> storage mechanism)
* Auto and manual model (class infos; metadata)
* Native drivers -- mapping straight to database: no mediators or conversions
* Bulk retrieve -- instantiate lots of objects with one query
* Lazy loading -- load objects or attributes only on demand
* TDD -- test driven development
* Lazarus wizards for manual mappings, manual model and persistence configuration
* API docs

There are some docs [here](http://pressobjects.github.io/jcore-api/docs/api/0.4/).

Some missing pieces before stabilize 0.4:

* Support of native data types
* Manual transactions
* Criteria improvement

## Dependency Injection Container

Classes which help you separate specifications from implementations, as well as give you the hability to override default implementation from a framework with your own classes.

* Declare your specification using interfaces
* Register at least one implementation
* Inject an implementation
* Support of qualifiers

Samples:

    TJCoreDIC.Register(IYourIntf, TYourImpl, jdsApplication);
    ...
    TJCoreDIC.Locate(IYourIntf, VAnIntfVar);

Using qualifier:

    TJCoreDIC.Register(IPayment, ‘cash’, TCashPayment, jdsApplication);
    TJCoreDIC.Register(IPayment, ‘plastic’, TPlasticPayment, jdsApplication);
    TJCoreDIC.Register(IPayment, ‘paypal’, TPaypalPayment, jdsApplication);
    ...
    TJCoreDIC.Locate(IPayment, ‘paypal’, VPayment);

## Expression Parser

An expression parser with an extensible function and operation library as well as support to variables.

The library is as fast as it can be: when the formula is changed, it is parsed in order to create an array with all operations, in the correct order. All results are referenced and reused without moves and copies. Point to pointers.

A simple calc:

    VExpression := TJCoreExpression.Create('2+2');
    try
      writeln(VExpression.VarValue);
    finally
      FreeAndNil(VExpression);
    end;

Using vars:

    VExpression := TJCoreExpression.Create;
    try
      VExpression.Vars.Variable['x'] := 2;
      VExpression.ParseExpression('2+x');
      writeln(VExpression.VarValue);
      VExpression.Vars.Variable['x'] := 4;
      writeln(VExpression.VarValue);
    finally
      FreeAndNil(VExpression);
    end;

User defined function:

    TSinFunction = class(TJCoreExpressionFunction)
    public
      function MaxParams: Integer; override;
      function MinParams: Integer; override;
      class function Name: string; override;
      procedure VarCalc; override;
    end;

    function TSinFunction.MaxParams: Integer;
    begin
      Result := 1;
    end;

    function TSinFunction.MinParams: Integer;
    begin
      Result := 1;
    end;

    class function TSinFunction.Name: string;
    begin
      Result := 'sin';
    end;

    procedure TSinFunction.VarCalc;
    begin
      Res^ := Sin(Params[0]^);
    end;

    begin
      JCoreExpressionLibrary.RegisterFunctions([TSinFunction]);
      VExpression := TJCoreExpression.Create('sin(30)');
      try
        writeln(VExpression.VarValue);
      finally
        FreeAndNil(VExpression);
      end;
    end;

User defined operation:

    TModOperation = class(TJCoreExpressionOperation)
    protected
      class function InternalOperatorToken: string; override;
    public
      function Priority: Byte; override;
      procedure VarCalc; override;
    end;

    class function TModOperation.InternalOperatorToken: string;
    begin
      Result := 'mod';
    end;

    function TModOperation.Priority: Byte;
    begin
      Result := 15;
    end;

    procedure TModOperation.VarCalc;
    begin
      Res^ := Val1^ mod Val2^;
    end;

    begin
      JCoreExpressionLibrary.RegisterOperations([TModOperation]);
      VExpression := TJCoreExpression.Create('3 mod 1');
      try
        writeln(VExpression.VarValue);
      finally
        FreeAndNil(VExpression);
      end;
    end;
