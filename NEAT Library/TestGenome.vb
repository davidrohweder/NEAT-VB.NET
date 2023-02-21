'David Rohweder 
'Start Date : June 8 2018
'Class Purpose: evalutate one generation from inital population size

Public Class TestGenome

    'init class variables
    Private nodeInnovation As Innovation
    Private connectionInnovation As Innovation
    Private random As New Random()
    Private C1 As Single = 1.0F
    Private C2 As Single = 1.0F
    Private C3 As Single = 0.4F
    Private DT As Single = 10.0F
    Private MUTATION_RATE As Single = 0.5
    Private ADD_CONNECTION_RATE As Single = 0.1
    Private ADD_NODE_RATE As Single = 0.1
    Private populationSize As Integer
    Private genomes As List(Of Genome)
    Private nextGenGenomes As List(Of Genome)
    Private specie As List(Of Species)
    Private mappedSpecies As Dictionary(Of Genome, Species)
    Private scoreMap As Dictionary(Of Genome, Single)
    Private highestScore As Single
    Private fittestGenomes As Genome
    Private fitnesslist() As Integer

    Public Sub New(ByVal populationSize As Integer, ByVal startingGenome As Genome, ByVal nodeInnovation As Innovation, ByVal connectionInnovation As Innovation)
        Me.populationSize = populationSize
        Me.nodeInnovation = nodeInnovation
        Me.connectionInnovation = connectionInnovation
        genomes = New List(Of Genome)(populationSize)
        For i As Integer = 0 To populationSize - 1
            genomes.Add(New Genome(startingGenome))
        Next i
        nextGenGenomes = New List(Of Genome)(populationSize)
        mappedSpecies = New Dictionary(Of Genome, Species)()
        scoreMap = New Dictionary(Of Genome, Single)()
        specie = New List(Of Species)()
    End Sub

    'run a generation
    Public Sub evaluate()
        ' Reset everything for next generation
        For Each s As Species In specie 'Each existing species is represented by a random genome inside the species from the previous generation
            s.reset(random)
        Next s
        scoreMap.Clear()
        mappedSpecies.Clear()
        nextGenGenomes.Clear()
        highestScore = Single.Epsilon
        fittestGenomes = Nothing
        fitnesslist = Nothing

        ' Place genomes into species
        For Each g As Genome In genomes 'In each generation, genomes are sequentially placed into species
            Dim foundSpecies As Boolean = False
            For Each s As Species In specie
                If g.compatibilityDistance(g, s.mascot, C1, C2, C3) < DT Then 'A given genome g in the current generation is placed in the first species in which g is compatible with the representative genome of that species
                    s.members.Add(g)
                    mappedSpecies.Add(g, s)
                    foundSpecies = True
                    Exit For
                End If
            Next
            If foundSpecies = False Then 'If g is not compatible with any existing species, a new species is created with g as its representative
                Dim newSpecies As New Species(Me, g)
                specie.Add(newSpecies)
                mappedSpecies.Add(g, newSpecies)
                newSpecies.mascot = g
            End If
        Next 

        'Remove unused species
        Dim tmpRUS As New List(Of Species)
        For Each s As Species In specie
            tmpRUS.Add(s)
        Next
        specie.Clear()
        For int As Integer = 0 To tmpRUS.Count - 1
            Dim s As Species = tmpRUS.ElementAt(int)
            If s.members.Count <> 0 Then
                specie.Add(s)
            End If
        Next

        ' Evaluate genomes and assign score
        For Each g As Genome In genomes
            Dim s As Species = mappedSpecies.Item(g) ' Get species of the genome
            Dim run As Run_NEAT = New Run_NEAT
            Dim score As Single = run.evaluateGenome(g)
            Dim adjustedScore As Single = score / mappedSpecies(g).members.Count()
            s.addAdjustedFitness(adjustedScore)
            s.fitnessPop.Add(New FitnessGenome(Me, g, adjustedScore))
            scoreMap.Add(g, adjustedScore)
            If score > highestScore Then
                highestScore = score
                fittestGenomes = g
            End If
        Next g

        ' put best genomes from each species into next generation
        Dim ints As Integer
        Dim fittestInSpecies As FitnessGenome
        For Each s As Species In specie
            Dim fitness As Single = s.fitnessPop(0).getfitness
            For Each g As Genome In s.members
                If fitness < s.fitnessPop(ints).getfitness Then
                    fittestInSpecies = s.fitnessPop(ints)
                    ints += 1
                Else
                    fittestInSpecies = s.fitnessPop(0)
                End If
            Next
            nextGenGenomes.Add(fittestInSpecies.getgenome)
        Next

        ' Breed the rest of the genomes 
        Do While nextGenGenomes.Count < populationSize ' replace removed genomes by randomly breeding
            Dim s As Species = getRandomSpeciesBiasedAjdustedFitness(random)
            Dim p1 As Genome = getRandomGenomeBiasedAdjustedFitness(s, random)
            Dim p2 As Genome = getRandomGenomeBiasedAdjustedFitness(s, random)
            Dim child As New Genome

            If scoreMap(p1) >= scoreMap(p2) Then
                child = child.crossover(p1, p2, random)
            Else
                child = child.crossover(p2, p1, random)
            End If
            If random.NextDouble() < MUTATION_RATE Then
                child.mutation(random)
            End If
            If random.NextDouble < ADD_CONNECTION_RATE Then
                child.addConnectionMutation(random, connectionInnovation)
            End If

            If random.NextDouble < ADD_NODE_RATE Then
                child.addNodeMutation(random, connectionInnovation, nodeInnovation)
            End If
            nextGenGenomes.Add(child)
        Loop

        genomes = nextGenGenomes
        nextGenGenomes = New List(Of Genome)()
    End Sub

    ' Selects a random species from the species list, where species with a higher total adjusted fitness have a higher chance of being selected
    Public Function getRandomSpeciesBiasedAjdustedFitness(ByVal rand As Random) As Species
        Dim completeWeight As Double = 0.0 ' sum of probablities of selecting each species - selection is more probable for species with higher fitness
        For Each s As Species In specie
            completeWeight += s.totalAdjustedFitness
        Next s
        Dim r As Double = rand.NextDouble * completeWeight
        Dim countWeight As Double = 0.0
        For Each s As Species In specie
            countWeight += s.totalAdjustedFitness
            If countWeight >= r Then
                Return s
            End If
        Next s
    End Function

    'Selects a random genome from the species chosen, where genomes with a higher adjusted fitness have a higher chance of being selected
    Public Function getRandomGenomeBiasedAdjustedFitness(ByVal selectFrom As Species, ByVal rand As Random) As Genome
        Dim completeWeight As Double = 0.0 ' sum of probablities of selecting each genome - selection is more probable for genomes with higher fitness
        For Each fg As FitnessGenome In selectFrom.fitnessPop
            completeWeight += fg.getfitness
        Next fg
        Dim r As Double = rand.NextDouble * completeWeight
        Dim countWeight As Double = 0.0
        For Each fg As FitnessGenome In selectFrom.fitnessPop
            countWeight += fg.getfitness
            If countWeight >= r Then
                Return fg.getgenome
            End If
        Next fg
    End Function

    Public Function SpeciesAmount() As Integer
        Return specie.Count
    End Function
    Public Function HighestFitness() As Single
        Return highestScore
    End Function

    Public Function FittestGenome() As Genome
        Return fittestGenomes
    End Function

    'contains the fitness of a genome rather than having the fitness as a property  
    Public Class FitnessGenome

        Private outerInstance As TestGenome
        Private fitness As Single
        Private genome As Genome

        Public Sub New(ByVal outerInstance As TestGenome, ByVal genome As Genome, ByVal fitness As Single)
            Me.outerInstance = outerInstance
            Me.genome = genome
            Me.fitness = fitness
        End Sub

        Public Function getfitness() As Single
            Return fitness
        End Function

        Public Function getgenome() As Genome
            Return genome
        End Function

    End Class

    Public Class Species
        Private outerInstance As TestGenome
        Public mascot As Genome 'represents specie
        Public members As List(Of Genome)
        Public fitnessPop As List(Of FitnessGenome)
        Public totalAdjustedFitness As Single = 0

        Public Sub New(ByVal outerInstance As TestGenome, ByVal mascot As Genome)
            Me.outerInstance = outerInstance
            Me.mascot = mascot
            members = New List(Of Genome)()
            members.Add(mascot)
            fitnessPop = New List(Of FitnessGenome)()
        End Sub

        Public Sub addAdjustedFitness(ByVal adjustedFitness As Single)
            totalAdjustedFitness += adjustedFitness
        End Sub

        '		 *	 Selects new random mascot + clear members + set totaladjustedfitness to 0		 
        Public Sub reset(ByVal rand As Random)
            Dim newMascotIndex As Integer = rand.Next(members.Count)
            mascot = members(newMascotIndex)
            members.Clear()
            fitnessPop.Clear()
            totalAdjustedFitness = 0
        End Sub

    End Class

End Class