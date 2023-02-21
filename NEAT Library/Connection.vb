'David Rohweder 
'Start Date : June 8 2018
'Class Purpose:

Public Class Connection

    'init class variables
    Private inNode As Integer
    Private outNode As Integer
    Private weight As Double
    Private expressed As Boolean
    Private innovation As Integer

    'constructor overloading
    Sub New(ByVal inNode As Integer, ByVal outNode As Integer, ByVal weight As Double, ByVal expressed As Boolean, ByVal innovation As Integer)
        Me.inNode = inNode
        Me.outNode = outNode
        Me.weight = weight
        Me.expressed = expressed
        Me.innovation = innovation
    End Sub

    Sub New(ByVal constuctor As Connection)
        inNode = constuctor.inNode
        outNode = constuctor.outNode
        weight = constuctor.weight
        expressed = constuctor.expressed
        innovation = constuctor.innovation
    End Sub

    'return class variables
    Public Function getinNode() As Integer
        Return inNode
    End Function

    Public Function getoutNode() As Integer
        Return outNode
    End Function

    Public Function getweight() As Double
        Return weight
    End Function

    Public Function getexpressedTrue() As Boolean
        Return expressed
    End Function

    Public Function getinnovation() As Integer
        Return innovation
    End Function

    'set class variables
    Public Sub isNOTexpressed()
        expressed = False
    End Sub

    Public Sub setweight(ByVal newweight As Double)
        weight = newweight
    End Sub

    '
    Public Function copy() As Connection
        Return New Connection(inNode, outNode, weight, expressed, innovation)
    End Function

End Class