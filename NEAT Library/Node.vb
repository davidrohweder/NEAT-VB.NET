'David Rohweder 
'Start Date : June 8 2018
'Class Purpose: 

Public Class Node

    'init class variables
    Private type As TYPES
    Private id As Integer = 0
    'enum stores a collection of constants
    Public Enum TYPES
        INPUT
        HIDDEN
        OUTPUT
    End Enum

    'constructor overloading
    Sub New(ByVal type As TYPES, ByVal id As Integer)
        Me.type = type
        Me.id = id
    End Sub

    Sub New(ByVal gene As Node)
        type = gene.type
        id = gene.id
    End Sub

    'Return class variables
    Public Function getTypes() As TYPES
        Return type
    End Function

    Public Function getid() As Integer
        Return id
    End Function

End Class