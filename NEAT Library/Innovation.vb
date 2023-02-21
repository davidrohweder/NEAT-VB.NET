'David Rohweder 
'Start Date : June 8 2018
'Class Purpose: To increment innovation counter to protect innovation from being deprecated

Public Class Innovation

    'init class variables
    Private current_innovation As Integer = 0

    'return class variables
    Public Function getInnovation() As Integer
        current_innovation += 1
        Return current_innovation
    End Function

End Class