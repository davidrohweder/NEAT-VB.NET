'David Rohweder 
'Start Date : June 8 2018
'Class Purpose: This class is used to store all of the connections and nodes of the whole ANN structure

Public Class Genome

    'init class variables 
    Private tmpList1 As List(Of Integer) = New List(Of Integer)() ' 
    Private tmpList2 As List(Of Integer) = New List(Of Integer)() '
    Private Perturb_Chance As Single = 0.9 ' rest is probability of assigning new weight
    Public connections As Dictionary(Of Integer, Connection)
    Public nodes As Dictionary(Of Integer, Node)

    'construtor overloading
    Public Sub New()
        nodes = New Dictionary(Of Integer, Node)()
        connections = New Dictionary(Of Integer, Connection)()
    End Sub

    Public Sub New(ByVal toBeCopied As Genome)
        nodes = New Dictionary(Of Integer, Node)()
        connections = New Dictionary(Of Integer, Connection)()

        For Each index As Integer? In toBeCopied.nodes.Keys
            nodes(index) = New Node(toBeCopied.nodes(index))
        Next index

        For Each index As Integer? In toBeCopied.connections.Keys
            connections(index) = New Connection(toBeCopied.connections(index))
        Next index
    End Sub

    'Adds a new node object to the node dictionary
    Public Sub addNode(ByVal gene As Node)
        nodes.Add(gene.getid, gene)
    End Sub

    'Adds a new connection object to the connection dictionary
    Public Sub addConnection(ByVal gene As Connection)
        connections.Add(gene.getinnovation, gene)
    End Sub

    'Returns connections dictionary
    Public Function getConnections() As Dictionary(Of Integer, Connection)
        Return connections
    End Function

    'Returns nodes dictionary
    Public Function getNode() As Dictionary(Of Integer, Node)
        Return nodes
    End Function

    'Executes a weight mutation, altering the values of weights together or individually
    Public Sub mutation(ByVal rand As Random)
        For Each connection As Connection In connections.Values
            If rand.NextDouble() < Perturb_Chance Then ' uniformly perturbing weights
                connection.setweight(connection.getweight * (rand.NextDouble() * 4.0F - 2.0F))
            Else ' assigning new weight
                connection.setweight(rand.NextDouble() * 4.0F - 2.0F)
            End If
        Next connection
    End Sub

    'Executes a connection mutation, adds a connection gene randomly within the genome
    Public Sub addConnectionMutation(ByVal rand As Random, ByVal innovation As Innovation)
        Dim tries As Integer = 0
        Dim success As Boolean = False

        Do While tries < 10 AndAlso success = False
            Dim node1 As Node = nodes.Item(rand.Next(nodes.Count - 1))
            Dim node2 As Node = nodes.Item(rand.Next(nodes.Count - 1))
            Dim weight As Single = rand.NextDouble() * 2.0F - 1.0F
            Dim reversed As Boolean = False

            If node1.getTypes = Node.TYPES.HIDDEN AndAlso node2.getTypes = Node.TYPES.INPUT Then
                reversed = True
            ElseIf node1.getTypes = Node.TYPES.OUTPUT AndAlso node2.getTypes = Node.TYPES.HIDDEN Then
                reversed = True
            ElseIf node1.getTypes = Node.TYPES.OUTPUT AndAlso node2.getTypes = Node.TYPES.INPUT Then
                reversed = True
            End If

            Dim connectionImpossible As Boolean = False 'Checks to see if connection is made between inputs or between outputs, only connections to and from hidden is viable

            If node1.getTypes = Node.TYPES.INPUT AndAlso node2.getTypes = Node.TYPES.INPUT Then
                connectionImpossible = True
            ElseIf node1.getTypes = Node.TYPES.OUTPUT AndAlso node2.getTypes = Node.TYPES.OUTPUT Then
                connectionImpossible = True
            End If

            Dim connectionExists As Boolean = False 'Checks to see if there is already an existing connection where it is trying to add one

            If connectionImpossible <> True Then
                For Each con As Connection In connections.Values
                    If con.getinNode = node1.getid AndAlso con.getoutNode = node2.getid Then ' existing connection
                        connectionExists = True
                        Exit For
                    ElseIf con.getinNode = node2.getid AndAlso con.getoutNode = node1.getid Then ' existing connection
                        connectionExists = True
                        Exit For
                    End If
                Next con
            End If
            If Not connectionExists OrElse Not connectionImpossible Then
                Dim newCon As New Connection(If(reversed, node2.getid, node1.getid), If(reversed, node1.getid, node2.getid), weight, True, innovation.getInnovation)
                connections(newCon.getinnovation) = newCon
                success = True
            End If
        Loop
    End Sub

    'Execute a node mutation, adds a node gene randomly within the genome
    Public Sub addNodeMutation(ByVal r As Random, ByVal connectionInnovation As Innovation, ByVal nodeInnovation As Innovation)
        Dim con As Connection = CType(connections.Values.ToArray()(r.Next(connections.Count)), Connection)

        Dim inNode As Node = nodes(con.getinNode)
        Dim outNode As Node = nodes(con.getoutNode)

        con.isNOTexpressed()
        Dim newNode As New Node(Node.TYPES.HIDDEN, nodes.Count)
        Dim inToNew As New Connection(inNode.getid, newNode.getid, 1.0, True, connectionInnovation.getInnovation)
        Dim newToOut As New Connection(newNode.getid, outNode.getid, con.getweight, True, connectionInnovation.getInnovation)

        addNode(newNode)
        addConnection(inToNew)
        addConnection(newToOut)
    End Sub

    'parent 1 more fit
    Public Function crossover(ByVal parent1 As Genome, ByVal parent2 As Genome, ByVal rand As Random) As Genome
        Dim child As New Genome()

        For Each parent1Node As Node In parent1.nodes.Values
            child.addNode(New Node(parent1Node))
        Next parent1Node

        For Each parent1Node As Connection In parent1.connections.Values
            If parent2.connections.ContainsKey(parent1Node.getinnovation) Then ' matching gene
                Dim childConGene As Connection = If(nextBoolean(), New Connection(parent1Node), New Connection(parent2.connections(parent1Node.getinnovation)))
                child.addConnection(childConGene)
            Else ' disjoint or excess gene
                Dim childConGene As New Connection(parent1Node)
                child.addConnection(childConGene)
            End If
        Next parent1Node

        Return child
    End Function

    Public Function nextBoolean() As Boolean
        Dim randbool As New Random
        Return Convert.ToBoolean(randbool.Next(0, 2))
    End Function

    Public Function compatibilityDistance(ByVal genome1 As Genome, ByVal genome2 As Genome, ByVal c1 As Single, ByVal c2 As Single, ByVal c3 As Single) As Single
        Dim excessGenes As Integer = countExcessGenes(genome1, genome2)
        Dim disjointGenes As Integer = countDisjointGenes(genome1, genome2)
        Dim avgWeightDiff As Single = 0 ' averageWeightDiff(genome1, genome2)
        Return excessGenes * c1 + disjointGenes * c2 + avgWeightDiff * c3
    End Function

    Public Function countMatchingGenes(ByVal genome1 As Genome, ByVal genome2 As Genome) As Integer
        Dim matchingGenes As Integer = 0
        Dim nodeKeys1 As List(Of Integer) = asSortedList(genome1.nodes.Keys, tmpList1)
        Dim nodeKeys2 As List(Of Integer) = asSortedList(genome2.nodes.Keys, tmpList2)
        Dim highestInnovation1 As Integer = nodeKeys1(nodeKeys1.Count - 1)
        Dim highestInnovation2 As Integer = nodeKeys2(nodeKeys2.Count - 1)
        Dim indices As Integer = Math.Max(highestInnovation1, highestInnovation2)

        For i As Integer = 0 To indices ' loop through genes -> i is innovation numbers
            Dim node1 As Node = genome1.nodes(i)
            Dim node2 As Node = genome2.nodes(i)
            If node1 IsNot Nothing AndAlso node2 IsNot Nothing Then
                ' both genomes has the gene w/ this innovation number
                matchingGenes += 1
            End If
        Next i

        Dim conKeys1 As List(Of Integer) = asSortedList(genome1.connections.Keys, tmpList1)
        Dim conKeys2 As List(Of Integer) = asSortedList(genome2.connections.Keys, tmpList2)

        highestInnovation1 = conKeys1(conKeys1.Count - 1)
        highestInnovation2 = conKeys2(conKeys2.Count - 1)

        indices = Math.Max(highestInnovation1, highestInnovation2)
        For i As Integer = 0 To indices ' loop through genes -> i is innovation numbers
            Dim connection1 As Connection = genome1.connections(i)
            Dim connection2 As Connection = genome2.connections(i)
            If connection1 IsNot Nothing AndAlso connection2 IsNot Nothing Then
                ' both genomes has the gene w/ this innovation number
                matchingGenes += 1
            End If
        Next i

        Return matchingGenes
    End Function

    'Returns the amount of "disjoint" or genes that are not new but one genome does not have - Purpose is used to help calculate compaability distance
    Public Function countDisjointGenes(ByVal genome1 As Genome, ByVal genome2 As Genome) As Integer
        Dim disjointGenes As Integer = 0 'init counter
        Dim nodeKeys1 As List(Of Integer) = asSortedList(genome1.nodes.Keys, tmpList1) 'gets list of nodes
        Dim nodeKeys2 As List(Of Integer) = asSortedList(genome2.nodes.Keys, tmpList2)
        Dim highestInnovation1 As Integer = nodeKeys1.Item(nodeKeys1.Count - 1) 'gets the highest amount of nodes the first genome has 
        Dim highestInnovation2 As Integer = nodeKeys2.Item(nodeKeys2.Count - 1) 'gets the highest amount of nodes the second genome has 
        Dim indices As Integer = Math.Max(highestInnovation1, highestInnovation2) 'finds out which genome has the highest amount of nodes
        Dim node1 As Node
        Dim node2 As Node
        Dim index As Integer = 0

        'loop through node gene 
        For index = 0 To indices
                Try
                node1 = genome1.getNode().Item(index) 'Gets node value from TValue in respective genome dictionary and returns the node corresponding to the TKey
                node2 = genome2.getNode().Item(index)
            Catch ex As KeyNotFoundException
                    If node1 Is Nothing AndAlso highestInnovation1 > index AndAlso node2 IsNot Nothing Then
                        ' genome 1 lacks gene, genome 2 has gene, genome 1 has more genes w/ higher innovation numbers
                        disjointGenes += 1
                    ElseIf node2 Is Nothing AndAlso highestInnovation2 > index AndAlso node1 IsNot Nothing Then
                        disjointGenes += 1
                    End If
                End Try
            Next

            Dim conKeys1 As List(Of Integer) = asSortedList(genome1.connections.Keys, tmpList1) 'gets list of connections
        Dim conKeys2 As List(Of Integer) = asSortedList(genome2.connections.Keys, tmpList2)

        highestInnovation1 = conKeys1(conKeys1.Count - 1)
        highestInnovation2 = conKeys2(conKeys2.Count - 1)
        indices = Math.Max(highestInnovation1, highestInnovation2)
        index = 0
        Dim connection1 As Connection
        Dim connection2 As Connection

        'loop through connection genes
        For index = 0 To indices
            Try
                connection1 = genome1.getConnections().Item(index)
                connection2 = genome2.getConnections().Item(index)
            Catch ex As KeyNotFoundException
                If connection1 Is Nothing AndAlso highestInnovation1 > index AndAlso connection2 IsNot Nothing Then
                        disjointGenes += 1
                    ElseIf connection2 Is Nothing AndAlso highestInnovation2 > index AndAlso connection1 IsNot Nothing Then
                        disjointGenes += 1
                    End If
                End Try
            Next

            Return disjointGenes
    End Function

    'Returns the amount of "Excess" genes or the amount amount of newer genes that one genome has that the other does not - Purpose calculating compatability distance
    Public Function countExcessGenes(ByVal genome1 As Genome, ByVal genome2 As Genome) As Integer
        Dim excessGenes As Integer = 0
        Dim nodeKeys1 As List(Of Integer) = asSortedList(genome1.getNode.Keys, tmpList1) 'gets list of nodes 
        Dim nodeKeys2 As List(Of Integer) = asSortedList(genome2.getNode.Keys, tmpList2)
        Dim highestInnovation1 As Integer = nodeKeys1.Item(nodeKeys1.Count - 1) 'gets the highest amount of nodes the first genome has 
        Dim highestInnovation2 As Integer = nodeKeys2.Item(nodeKeys2.Count - 1) 'gets the highest amount of nodes the second genome has
        Dim indices As Integer = Math.Max(highestInnovation1, highestInnovation2) 'finds out which genome has the highest amount of nodes
        Dim index As Integer = 0
        Dim node1 As Node
        Dim node2 As Node

        'loop through node genes
        For index = 0 To indices
            Try
                node1 = genome1.getNode().Item(index) 'Gets node value from TValue in respective genome dictionary and returns the node corresponding to the TKey
                node2 = genome2.getNode().Item(index)
            Catch ex As KeyNotFoundException
                If node1 Is Nothing AndAlso highestInnovation1 < index AndAlso node2 IsNot Nothing Then
                excessGenes += 1
            ElseIf node2 Is Nothing AndAlso highestInnovation2 < index AndAlso node1 IsNot Nothing Then
                excessGenes += 1
            End If
        End Try
        Next

        Dim conKeys1 As List(Of Integer) = asSortedList(genome1.connections.Keys, tmpList1) 'gets list of connections
        Dim conKeys2 As List(Of Integer) = asSortedList(genome2.connections.Keys, tmpList2)

        highestInnovation1 = conKeys1(conKeys1.Count - 1)
        highestInnovation2 = conKeys2(conKeys2.Count - 1)
        indices = Math.Max(highestInnovation1, highestInnovation2)
        index = 0
        Dim connection1 As Connection
        Dim connection2 As Connection

        'loop through connection genes
        For index = 0 To indices
            Try
                connection1 = genome1.getConnections().Item(index) 'Gets connection value from TValue in respective genome dictionary and returns the node corresponding to the TKey
                connection2 = genome2.getConnections().Item(index)
            Catch ex As KeyNotFoundException
                If connection1 Is Nothing AndAlso highestInnovation1 < index AndAlso connection2 IsNot Nothing Then
                    excessGenes += 1
                ElseIf connection2 Is Nothing AndAlso highestInnovation2 < index AndAlso connection1 IsNot Nothing Then
                    excessGenes += 1
                End If
            End Try
        Next

        Return excessGenes
    End Function

    Public Function averageWeightDiff(ByVal genome1 As Genome, ByVal genome2 As Genome) As Single
        Dim matchingGenes As Integer = 0
        Dim weightDifference As Single = 0

        Dim conKeys1 As List(Of Integer) = asSortedList(genome1.connections.Keys, tmpList1)
        Dim conKeys2 As List(Of Integer) = asSortedList(genome2.connections.Keys, tmpList2)

        Dim highestInnovation1 As Integer = conKeys1(conKeys1.Count - 1)
        Dim highestInnovation2 As Integer = conKeys2(conKeys2.Count - 1)
        Dim indices As Integer = Math.Max(highestInnovation1, highestInnovation2)
        Dim index As Integer = 0
        Dim connection1 As Connection
        Dim connection2 As Connection

        'loop through connection weights
        For index = 0 To indices ' loop through genes -> i is innovation number
            Try
                connection1 = genome1.connections(index)
                connection2 = genome2.connections(index)
            Catch ex As KeyNotFoundException
                If connection1 IsNot Nothing AndAlso connection2 IsNot Nothing Then
                    ' both genomes has the gene w/ this innovation number
                    matchingGenes += 1
                    weightDifference += Math.Abs(connection1.getweight - connection2.getweight)
                End If
            End Try
        Next

        Return weightDifference / matchingGenes
    End Function

    Private Function asSortedList(ByVal c As ICollection(Of Integer), ByVal list As List(Of Integer)) As List(Of Integer)
        list.Clear()
        CType(list, List(Of Integer)).AddRange(c)
        list.Sort()
        Return list
    End Function

End Class