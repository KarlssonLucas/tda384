package amazed.solver;

import amazed.maze.Maze;

import java.util.List;
import java.util.ArrayList;
import java.util.Map;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Set;
import java.util.Stack;
import java.util.concurrent.ConcurrentSkipListSet;
import java.util.concurrent.ConcurrentSkipListMap;

/**
 * <code>ForkJoinSolver</code> implements a solver for
 * <code>Maze</code> objects using a fork/join multi-thread
 * depth-first search.
 * <p>
 * Instances of <code>ForkJoinSolver</code> should be run by a
 * <code>ForkJoinPool</code> object.
 */


public class ForkJoinSolver
    extends SequentialSolver
{

    ConcurrentSkipListMap<Integer, Integer> predecessor;
    ConcurrentSkipListSet<Integer> visited;
    Stack<Integer> frontier;
    Integer start;
    int player;

    /**
     * Creates a solver that searches in <code>maze</code> from the
     * start node to a goal.
     *
     * @param maze   the maze to be searched
     */
    public ForkJoinSolver(Maze maze)
    {
        super(maze);
        
    }

    /**
     * Creates a solver that searches in <code>maze</code> from the
     * start node to a goal, forking after a given number of visited
     * nodes.
     *
     * @param maze        the maze to be searched
     * @param forkAfter   the number of steps (visited nodes) after
     *                    which a parallel task is forked; if
     *                    <code>forkAfter &lt;= 0</code> the solver never
     *                    forks new tasks
     */
    public ForkJoinSolver(Maze maze, int forkAfter)
    {
        this(maze);
        this.forkAfter = forkAfter;
        predecessor = new ConcurrentSkipListMap<>();
        visited = new ConcurrentSkipListSet<>();
        frontier = new Stack<>();
        start = this.maze.start();
        frontier.add(start);
        this.player = maze.newPlayer(start);
    }

    public ForkJoinSolver(Maze maze, int forkAfter, ConcurrentSkipListMap predecessor, ConcurrentSkipListSet visited, Stack frontier, Integer start) {
        super(maze);
        this.predecessor = predecessor;
        this.visited = visited;
        this.frontier = frontier;
        this.forkAfter = forkAfter;
        this.start = start;
        this.player = maze.newPlayer(start);
    }

    /**
     * Searches for and returns the path, as a list of node
     * identifiers, that goes from the start node to a goal node in
     * the maze. If such a path cannot be found (because there are no
     * goals, or all goals are unreacheable), the method returns
     * <code>null</code>.
     *
     * @return   the list of node identifiers from the start node to a
     *           goal node in the maze; <code>null</code> if such a path cannot
     *           be found.
     */
    @Override
    public List<Integer> compute()
    {
        return parallelSearch();
    }

    private List<Integer> parallelSearch() {

        System.out.println(frontier.size());
        Integer current = frontier.pop();
        visited.add(current);
        maze.move(player, current);

        if (maze.hasGoal(current)) {
            return pathFromTo(start, current);
        }

        Set<Integer> adj = maze.neighbors(current);
        adj.removeIf(node -> visited.contains(node));
        for (Integer n : adj) {
            predecessor.put(n, current);
        }

        
        if (adj.size() > 1) {
            for(Integer node : adj) {
                Stack<Integer> newFrontier = new Stack<>();
                newFrontier.add(node);
                new ForkJoinSolver(maze, forkAfter, predecessor, visited, newFrontier, start).fork();
                
            }
        } else if (adj.size() == 1) {
            return parallelSearch();
        } else {
            join();
        }

        return null;
    }
}
