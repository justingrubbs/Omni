package Sudoku;
import java.io.*;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Random;

public class Sudoku {


    public final int[][] grid = new int [9][9];
    public HashMap<Integer, Integer> answerKey = new HashMap<>(81);
    public HashMap<Integer, Integer> currentKey = new HashMap<>(81);
    public HashMap<Integer, int[]> intersectionKey = new HashMap<>(81);
    public HashMap<Integer, ArrayList<Integer>> pencilKey = new HashMap<>(81);
    public HashMap<Integer, ArrayList<Integer>> matchingCellKey = new HashMap<>(9);
    public ArrayList<Integer> staticCells = new ArrayList<>(0);
    public ArrayList<Integer> wrongCells = new ArrayList<>(0);
    public boolean gameOver = false;


    public Sudoku(Difficulty difficulty) {
        int count = 1;
        for (int i = 0; i < grid.length; i++) {
            for (int n = 0; n < grid[0].length; n++) {
                grid[n][i] = count;
                count++;
            }
        }
        textToHashMaps(difficulty);
        initializeSimilarCellMap();
    }

    public int getLength1() {
        return grid.length;
    }

    public int getLength2() {
        return grid[0].length;
    }

    public int[] getIntersectionRows(int[] array, int i) {
        int n = i % 9;
        if (n == 1) {
            array[0] = i + 3;
            array[1] = i + 4;
            array[2] = i + 5;
            array[3] = i + 6;
            array[4] = i + 7;
            array[5] = i + 8;
        }
        else if (n == 2) {
            array[0] = i + 2;
            array[1] = i + 3;
            array[2] = i + 4;
            array[3] = i + 5;
            array[4] = i + 6;
            array[5] = i + 7;
        }
        else if (n == 3) {
            array[0] = i + 1;
            array[1] = i + 2;
            array[2] = i + 3;
            array[3] = i + 4;
            array[4] = i + 5;
            array[5] = i + 6;
        }
        else if (n == 4) {
            array[0] = i - 3;
            array[1] = i - 2;
            array[2] = i - 1;
            array[3] = i + 3;
            array[4] = i + 4;
            array[5] = i + 5;
        }
        else if (n == 5) {
            array[0] = i - 4;
            array[1] = i - 3;
            array[2] = i - 2;
            array[3] = i + 2;
            array[4] = i + 3;
            array[5] = i + 4;
        }
        else if (n == 6) {
            array[0] = i - 5;
            array[1] = i - 4;
            array[2] = i - 3;
            array[3] = i + 1;
            array[4] = i + 2;
            array[5] = i + 3;
        }
        else if (n == 7) {
            array[0] = i - 6;
            array[1] = i - 5;
            array[2] = i - 4;
            array[3] = i - 3;
            array[4] = i - 2;
            array[5] = i - 1;
        }
        else if (n == 8) {
            array[0] = i - 7;
            array[1] = i - 6;
            array[2] = i - 5;
            array[3] = i - 4;
            array[4] = i - 3;
            array[5] = i - 2;
        }
        else if (n == 0) {
            array[0] = i - 8;
            array[1] = i - 7;
            array[2] = i - 6;
            array[3] = i - 5;
            array[4] = i - 4;
            array[5] = i - 3;
        }
        return array;
    }

    public int[] getIntersectionColumns(int[] array, int i) {
        if ((i >= 1) && (i <= 9)) {
            array[6] = i + 27;
            array[7] = i + 36;
            array[8] = i + 45;
            array[9] = i + 54;
            array[10] = i + 63;
            array[11] = i + 72;
        }
        else if ((i >= 9) && (i <= 18)) {
            array[6] = i + 18;
            array[7] = i + 27;
            array[8] = i + 36;
            array[9] = i + 45;
            array[10] = i + 54;
            array[11] = i + 63;
        }
        else if ((i >= 18) && (i <= 27)) {
            array[6] = i + 9;
            array[7] = i + 18;
            array[8] = i + 27;
            array[9] = i + 36;
            array[10] = i + 45;
            array[11] = i + 54;
        }
        else if ((i >= 27) && (i <= 36)) {
            array[6] = i - 27;
            array[7] = i - 18;
            array[8] = i - 9;
            array[9] = i + 27;
            array[10] = i + 36;
            array[11] = i + 45;
        }
        else if ((i >= 36) && (i <= 45)) {
            array[6] = i - 36;
            array[7] = i - 27;
            array[8] = i - 18;
            array[9] = i + 18;
            array[10] = i + 27;
            array[11] = i + 36;
        }
        else if ((i >= 45) && (i <= 54)) {
            array[6] = i - 45;
            array[7] = i - 36;
            array[8] = i - 27;
            array[9] = i + 9;
            array[10] = i + 18;
            array[11] = i + 27;
        }
        else if ((i >= 54) && (i <= 63)) {
            array[6] = i - 54;
            array[7] = i - 45;
            array[8] = i - 36;
            array[9] = i - 27;
            array[10] = i - 18;
            array[11] = i - 9;
        }
        else if ((i >= 63) && (i <= 72)) {
            array[6] = i - 63;
            array[7] = i - 54;
            array[8] = i - 45;
            array[9] = i - 36;
            array[10] = i - 27;
            array[11] = i - 18;
        } else if ((i >= 72) && (i <= 81)) {
            array[6] = i - 72;
            array[7] = i - 63;
            array[8] = i - 54;
            array[9] = i - 45;
            array[10] = i - 36;
            array[11] = i - 27;
        }
        return array;
    }

    public int[] getIntersectionBoxes(int[] array, int i) {
        int[] temp = new int[9];
        if (((i >= 1) && (i <= 3)) || ((i >= 10) && (i <= 12)) || ((i >= 19) && (i <= 21))) {
            int index = 12;
            temp[0] = 1;
            temp[1] = 2;
            temp[2] = 3;
            temp[3] = 10;
            temp[4] = 11;
            temp[5] = 12;
            temp[6] = 19;
            temp[7] = 20;
            temp[8] = 21;
            for (int n = 0; n < temp.length; n++) {
                if (i != temp[n]) {
                    array[index] = temp[n];
                    index++;
                }
            }
        }
        else if (((i >= 4) && (i <= 6)) || ((i >= 13) && (i <= 15)) || ((i >= 22) && (i <= 24))) {
            int index = 12;
            temp[0] = 4;
            temp[1] = 5;
            temp[2] = 6;
            temp[3] = 13;
            temp[4] = 14;
            temp[5] = 15;
            temp[6] = 22;
            temp[7] = 23;
            temp[8] = 24;
            for (int n = 0; n < temp.length; n++) {
                if (i != temp[n]) {
                    array[index] = temp[n];
                    index++;
                }
            }
        }
        if (((i >= 7) && (i <= 9)) || ((i >= 16) && (i <= 18)) || ((i >= 25) && (i <= 27))) {
            int index = 12;
            temp[0] = 7;
            temp[1] = 8;
            temp[2] = 9;
            temp[3] = 16;
            temp[4] = 17;
            temp[5] = 18;
            temp[6] = 25;
            temp[7] = 26;
            temp[8] = 27;
            for (int n = 0; n < temp.length; n++) {
                if (i != temp[n]) {
                    array[index] = temp[n];
                    index++;
                }
            }
        }
        if (((i >= 28) && (i <= 30)) || ((i >= 37) && (i <= 39)) || ((i >= 46) && (i <= 48))) {
            int index = 12;
            temp[0] = 28;
            temp[1] = 29;
            temp[2] = 30;
            temp[3] = 37;
            temp[4] = 38;
            temp[5] = 39;
            temp[6] = 46;
            temp[7] = 47;
            temp[8] = 48;
            for (int n = 0; n < temp.length; n++) {
                if (i != temp[n]) {
                    array[index] = temp[n];
                    index++;
                }
            }
        }
        if (((i >= 31) && (i <= 33)) || ((i >= 40) && (i <= 42)) || ((i >= 49) && (i <= 51))) {
            int index = 12;
            temp[0] = 31;
            temp[1] = 32;
            temp[2] = 33;
            temp[3] = 40;
            temp[4] = 41;
            temp[5] = 42;
            temp[6] = 49;
            temp[7] = 50;
            temp[8] = 51;
            for (int n = 0; n < temp.length; n++) {
                if (i != temp[n]) {
                    array[index] = temp[n];
                    index++;
                }
            }
        }
        if (((i >= 34) && (i <= 36)) || ((i >= 43) && (i <= 45)) || ((i >= 52) && (i <= 54))) {
            int index = 12;
            temp[0] = 34;
            temp[1] = 35;
            temp[2] = 36;
            temp[3] = 43;
            temp[4] = 44;
            temp[5] = 45;
            temp[6] = 52;
            temp[7] = 53;
            temp[8] = 54;
            for (int n = 0; n < temp.length; n++) {
                if (i != temp[n]) {
                    array[index] = temp[n];
                    index++;
                }
            }
        }
        if (((i >= 55) && (i <= 57)) || ((i >= 64) && (i <= 66)) || ((i >= 73) && (i <= 75))) {
            int index = 12;
            temp[0] = 55;
            temp[1] = 56;
            temp[2] = 57;
            temp[3] = 64;
            temp[4] = 65;
            temp[5] = 66;
            temp[6] = 73;
            temp[7] = 74;
            temp[8] = 75;
            for (int n = 0; n < temp.length; n++) {
                if (i != temp[n]) {
                    array[index] = temp[n];
                    index++;
                }
            }
        }
        if (((i >= 58) && (i <= 60)) || ((i >= 67) && (i <= 69)) || ((i >= 76) && (i <= 78))) {
            int index = 12;
            temp[0] = 58;
            temp[1] = 59;
            temp[2] = 60;
            temp[3] = 67;
            temp[4] = 68;
            temp[5] = 69;
            temp[6] = 76;
            temp[7] = 77;
            temp[8] = 78;
            for (int n = 0; n < temp.length; n++) {
                if (i != temp[n]) {
                    array[index] = temp[n];
                    index++;
                }
            }
        }
        if (((i >= 61) && (i <= 63)) || ((i >= 70) && (i <= 72)) || ((i >= 79) && (i <= 81))) {
            int index = 12;
            temp[0] = 61;
            temp[1] = 62;
            temp[2] = 63;
            temp[3] = 70;
            temp[4] = 71;
            temp[5] = 72;
            temp[6] = 79;
            temp[7] = 80;
            temp[8] = 81;
            for (int n = 0; n < temp.length; n++) {
                if (i != temp[n]) {
                    array[index] = temp[n];
                    index++;
                }
            }
        }
        return array;
    }

    public void initializeSimilarCellMap() {
        for (int i = 1; i < 10; i++) {
            ArrayList<Integer> temp = new ArrayList<>(0);
            for (Integer key : currentKey.keySet()) {
                if (currentKey.get(key) == i) {
                    temp.add(key);
                }
            }
            matchingCellKey.put(i, temp);
        }
    }

    public void getIntersectionMap(int i) {
        int[] array = new int[20];
        array = getIntersectionRows(array, i);
        array = getIntersectionColumns(array, i);
        array = getIntersectionBoxes(array, i);
        intersectionKey.put(i, array);
    }

    public boolean isCurrentKeyFilled() {
        return !currentKey.containsValue(0);
    }

    public boolean gameWonOrLost() {
        return (wrongCells.size() == 0);
    }

    public void textToHashMaps(Difficulty difficulty) {
        Random rand = new Random();
        InputStream f;
        int randomInt = rand.nextInt(0, 2);
        try {
//            https://www.tutorialspoint.com/java/java_files_io.htm
            if (difficulty.getDifficulty() == 1) {
                f = new FileInputStream("src/EASY/EasyGrid_" + randomInt + ".txt");
            } else if (difficulty.getDifficulty() == 2) {
                f = new FileInputStream("src/MEDIUM/MediumGrid_" + randomInt + ".txt");
            } else if (difficulty.getDifficulty() == 3) {
                f = new FileInputStream("src/HARD/HardGrid_" + randomInt + ".txt");
            } else {
                f = new FileInputStream("src/EXPERT/ExpertGrid_" + randomInt + ".txt");
            }
            int size = f.available() + 1;
            for(int i = 1; i < size; i++) {
                getIntersectionMap(i);
                int n = f.read();
                currentKey.put(i, n - 48);
                if (n - 48 != 0) {
                    staticCells.add(i);
                }
            }
            f.close();
            if (difficulty.getDifficulty() == 1) {
                f = new FileInputStream("src/EASY/EasyGridKey_" + randomInt + ".txt");
            } else if (difficulty.getDifficulty() == 2) {
                f = new FileInputStream("src/MEDIUM/MediumGridKey_" + randomInt + ".txt");
            } else if (difficulty.getDifficulty() == 3) {
                f = new FileInputStream("src/HARD/HardGridKey_" + randomInt + ".txt");
            } else {
                f = new FileInputStream("src/EXPERT/ExpertGridKey_" + randomInt + ".txt");
            }
            size = f.available() + 1;
            for(int i = 1; i < size; i++) {
                int n = f.read();
                answerKey.put(i, n - 48);
            }
            f.close();
        } catch (IOException ioe) {
            System.out.println("IOException Error");
        }
    }
}