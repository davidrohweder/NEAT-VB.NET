'David Rohweder 
'Start Date : June 8 2018
'Module Purpose: To implement the NEAT library to succesully making a working ANN from inputs given

Public Class Run_NEAT

    'init class variable
    Private outputnode_cervo(59) As Integer 'Specifies the amount of angles * the amount of cervos that should be represented as output nodes

    Sub Startup()
        Console.WriteLine("Enter Authorized Credentials")
        Dim str As String = Convert.ToString(Console.ReadLine)
        Console.Write("Enter population number: ")
        Dim popsize As Integer = Convert.ToInt32(Console.ReadLine)
        Console.Write("Enter generation amount: ") 'how many generations are going to take place
        Dim generationamount As Integer = Convert.ToInt32(Console.ReadLine)
        Dim bigtimeerror As Boolean
        ' If str = "David Rohweder" Then
        Do While IsNumeric(generationamount) AndAlso generationamount > 0 AndAlso IsNumeric(popsize) AndAlso popsize > 0 AndAlso bigtimeerror = False
                'init class var
                Dim innovation_node As Innovation = New Innovation 'keeps track and submit all of the innovations that are node genes
                Dim innovation_connection As Innovation = New Innovation 'keeps track and submit all of the innovations that are connection genes
                Dim genome As Genome = New Genome 'Sends out the starting genome to the TestGenome constructor that will use its argument to begin the first species and genome innovation
            genome.addNode(New Node(Node.TYPES.INPUT, 0))
            For Each index As Integer In outputnode_cervo
                    genome.addNode(New Node(Node.TYPES.OUTPUT, innovation_node.getInnovation))
                Next
            genome.addConnection(New Connection(1, 1, 0.05, True, 0))
            Dim test_genome As New TestGenome(popsize, genome, innovation_node, innovation_connection)
            For index As Integer = 0 To generationamount
                test_genome.evaluate()
                Console.WriteLine(index)
            Next
            bigtimeerror = True
            If bigtimeerror = False Then
                Console.WriteLine(str & " Program has succesfully completed " & generationamount & " Generations, with population size of " & popsize & " : higest fitness " & test_genome.HighestFitness & ", species amount: " & test_genome.SpeciesAmount)
                Console.Write("Enter population number: ")
                popsize = Convert.ToInt32(Console.ReadLine)
                Console.Write("Enter generation number: ")
                generationamount = Convert.ToInt32(Console.ReadLine)
            End If
        Loop
        ' End If
    End Sub

    'The purpose of this procedure is to connect to an individual cervo and rotate it from the given arguements 
    Public Sub rotate_cervo(ByVal index As Integer, ByVal degree As Integer)

    End Sub

    Public Function evaluateGenome(ByVal genome As Genome) As Single
        Dim fitness As Double = genome.getNode.Count
        Return fitness
    End Function

End Class