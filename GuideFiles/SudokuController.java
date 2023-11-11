package Sudoku;

import javafx.fxml.FXML;
import javafx.scene.control.Button;
import javafx.scene.control.Label;
import javafx.scene.input.KeyCode;
import javafx.scene.layout.Pane;
import javafx.scene.paint.Color;
import javafx.scene.shape.Rectangle;
import javafx.scene.text.Font;

import java.util.ArrayList;
import java.util.HashMap;


public class SudokuController {


    private Sudoku sudoku;
    private boolean showIntersects = true;
    private boolean showMatchingCells = true;
    private boolean showErrors = true;
    private boolean gridFilled = false;
    private int currentCellIndex;
    private HashMap<Integer, Label> labelMap = new HashMap<>(81);
    private HashMap<Integer, Rectangle> cellMap = new HashMap<>(81);
    @FXML
    private Pane gameGrid;
    @FXML
    private Button newGame;
    @FXML
    private Button toggleIntersects;
    @FXML
    private Button toggleMatchingCells;
    @FXML
    private Button toggleErrors;
    @FXML
    private Button finishGame;
    @FXML
    private Button easy;
    @FXML
    private Button medium;
    @FXML
    private Button hard;
    @FXML
    private Button expert;
    @FXML
    private Label gameWonOrLost;


    @FXML
    public void initialize() {
        easy.setVisible(false);
        medium.setVisible(false);
        hard.setVisible(false);
        expert.setVisible(false);
        finishGame.setOpacity(.5);
        setButtonOpacity(.5);
    }

    @FXML
    public void toggleDifficultyVisibilty() {
        easy.setVisible(!easy.isVisible());
        medium.setVisible(!medium.isVisible());
        hard.setVisible(!hard.isVisible());
        expert.setVisible(!expert.isVisible());
    }

    @FXML
    public void setButtonOpacity(double value) {
        toggleIntersects.setOpacity(value);
        toggleMatchingCells.setOpacity(value);
        toggleErrors.setOpacity(value);
    }

    @FXML
    public void resetEasy() {
        sudoku = new Sudoku(Difficulty.EASY);
        gameWonOrLost.setText("");
        toggleDifficultyVisibilty();
        setButtonOpacity(1);
        drawGrid();
    }

    @FXML
    public void resetMedium() {
        sudoku = new Sudoku(Difficulty.MEDIUM);
        gameWonOrLost.setText("");
        toggleDifficultyVisibilty();
        setButtonOpacity(1);
        drawGrid();
    }

    @FXML
    public void resetHard() {
        sudoku = new Sudoku(Difficulty.HARD);
        gameWonOrLost.setText("");
        toggleDifficultyVisibilty();
        setButtonOpacity(1);
        drawGrid();
    }

    @FXML
    public void resetExpert() {
        sudoku = new Sudoku(Difficulty.EXPERT);
        gameWonOrLost.setText("");
        toggleDifficultyVisibilty();
        setButtonOpacity(1);
        drawGrid();
    }

    @FXML
    public void toggleIntersects() {
        if (toggleIntersects.getOpacity() == 1) {
            if (!sudoku.gameOver) {
                showIntersects = !showIntersects;
                if (showIntersects) {
                    toggleIntersects.setText("Disable Highlighting Intersections");
                } else {
                    toggleIntersects.setText("Enable Highlighting Intersections");
                }
                gridClicked(currentCellIndex);
            }
        }
    }

    @FXML
    public void toggleMatchingCells() {
        if (toggleMatchingCells.getOpacity() == 1) {
            if (!sudoku.gameOver) {
                showMatchingCells = !showMatchingCells;
                if (showMatchingCells) {
                    toggleMatchingCells.setText("Disable Highlighting Matching Cells");
                } else {
                    toggleMatchingCells.setText("Enable \nHighlighting Matching Cells");
                }
                gridClicked(currentCellIndex);
            }
        }
    }

    @FXML
    public void toggleErrors() {
        if (toggleErrors.getOpacity() == 1) {
            if (!sudoku.gameOver) {
                showErrors = !showErrors;
                if (showErrors) {
                    toggleErrors.setText("Disable Highlighting Errors");
                } else {
                    toggleErrors.setText("Enable \nHighlighting Errors");
                }
                gridClicked(currentCellIndex);
            }
        }
    }

    @FXML
    public void finishedGame() {
        if (finishGame.getOpacity() == 1) {
            if (!sudoku.gameOver) {
                resetCellColors();
                setButtonOpacity(.5);
                finishGame.setOpacity(.5);
                sudoku.gameOver = true;
                if (sudoku.gameWonOrLost()) {
                    gameWonOrLost.setText("You have won the game!");
                } else {
                    gameWonOrLost.setText("You have lost the game!");
                    highlightWrongCells();
                }
            }
        }
    }

    @FXML
    public void setShowIntersects(int[] intersectArray) {
        if (showIntersects) {
            for (int k = 0; k < intersectArray.length; k++) {
                cellMap.get(intersectArray[k]).setFill(Color.LIGHTGRAY);
            }
        }
    }

    @FXML
    public void setMatchingCells(int index) {
        if (showMatchingCells) {
            if (sudoku.currentKey.get(index) != 0) {
                ArrayList<Integer> temp = sudoku.matchingCellKey.get(sudoku.currentKey.get(index));
                for (int i = 0; i < temp.size(); i++) {
                    if (temp.get(i) != index) {
                        cellMap.get(temp.get(i)).setFill(Color.LAVENDER);
                    }
                }
            }
        }
    }

    @FXML
    public void addMatchingCell(int index) {
        ArrayList<Integer> temp = sudoku.matchingCellKey.get(sudoku.currentKey.get(index));
        if (sudoku.currentKey.get(index) != 0) {
            if (!temp.contains(index)) {
                temp.add(index);
            }
        }
    }

    @FXML
    public void removeMatchingCell(int index) {
        ArrayList<Integer> temp = sudoku.matchingCellKey.get(sudoku.currentKey.get(index));
        if (sudoku.currentKey.get(index) != 0) {
            for (int i = 0; i < temp.size(); i++) {
                if (temp.get(i) == index) {
                    temp.remove(i);
                }
            }
        }
    }

    @FXML
    public void setLabel(Label label, int index) {
        label.setFont(new Font("Arial", 30));
        if (sudoku.staticCells.contains(index)) {
            label.setTextFill(Color.BLACK);
        } else {
            label.setTextFill(Color.ROYALBLUE);
        }
        labelMap.put(index, label);
        gameGrid.getChildren().add(label);
    }

    @FXML
    public void labelChanger() {
        for (int i = 0; i < sudoku.getLength1(); i++) {
            for (int n = 0; n < sudoku.getLength2(); n++) {
                Label label = labelMap.get((9 * n) + (i + 1));
                if (sudoku.currentKey.get((9 * n) + (i + 1)) == 0) {
                    label.setText("");
                } else {
                    label.setText("" + sudoku.currentKey.get((9 * n) + (i + 1)));
                }
                label.setTranslateX(i * 50 + (50 / 3) - 1);
                label.setTranslateY(n * 50 + (50 / 3) - 9);
                labelMap.put((9 * n) + (i + 1), label);
            }
        }
    }

    @FXML
    public void resetCellColors() {
        for (int j = 1; j < 82; j++) {
            cellMap.get(j).setFill(Color.WHITE);
        }
    }

    @FXML
    public void cellSelected(Rectangle cell) {
        cell.setFill(Color.LIGHTSTEELBLUE);
    }

    public void editWrongList(int cellIndex) {
        if ((sudoku.currentKey.get(cellIndex) != 0) && (sudoku.currentKey.get(cellIndex) != sudoku.answerKey.get(cellIndex))) {
            if (!sudoku.wrongCells.contains(cellIndex)) {
                sudoku.wrongCells.add(cellIndex);
            }
        } else if ((sudoku.currentKey.get(cellIndex) == 0) || (sudoku.currentKey.get(cellIndex) == sudoku.answerKey.get(cellIndex))){
//            https://stackoverflow.com/questions/21795376/java-how-to-remove-an-integer-item-in-an-arraylist
            sudoku.wrongCells.remove((Integer) cellIndex);
        }
    }

    @FXML
    public void changeCell(int input, int index) {
        if (!sudoku.gameOver) {
            if (sudoku.currentKey.get(index) == input) {
                input = 0;
                removeMatchingCell(index);
            }
            sudoku.currentKey.put(index, input);
            editWrongList(index);
            addMatchingCell(index);
            labelChanger();
            if (sudoku.isCurrentKeyFilled()) {
                finishGame.setOpacity(1);
            } else {
                finishGame.setOpacity(.5);
            }
            gridClicked(index);
        }
    }

    @FXML
    public void highlightWrongCells() {
        if ((showErrors) || (sudoku.gameOver)) {
            if (!sudoku.wrongCells.isEmpty()) {
                for (int i = 0; i < sudoku.wrongCells.size(); i++) {
                    cellMap.get(sudoku.wrongCells.get(i)).setFill(Color.PALEVIOLETRED);
                }
            }
        }
    }

    @FXML
    public void gridClicked(int cellIndex) {
        if (!sudoku.gameOver) {
            int[] intersectArray = sudoku.intersectionKey.get(cellIndex);
            currentCellIndex = cellIndex;
            resetCellColors();
            setShowIntersects(intersectArray);
            cellSelected(cellMap.get(cellIndex));
            setMatchingCells(cellIndex);
            highlightWrongCells();
            cellMap.get(cellIndex).requestFocus();
            cellMap.get(cellIndex).setOnKeyPressed(event2 -> {
                if (event2.getCode().equals(KeyCode.DOWN)) {
                    if (cellIndex < 73) {
                        gridClicked(cellIndex + 9);
                    }
                } else if (event2.getCode().equals(KeyCode.UP)) {
                    if (cellIndex > 9) {
                        gridClicked(cellIndex - 9);
                    }
                } else if (event2.getCode().equals(KeyCode.LEFT)) {
                    if (cellIndex % 9 != 1) {
                        gridClicked(cellIndex - 1);
                    }
                } else if (event2.getCode().equals(KeyCode.RIGHT)) {
                    if (cellIndex % 9 != 0) {
                        gridClicked(cellIndex + 1);
                    }
                }
                if (!sudoku.staticCells.contains(cellIndex)) {
                    if ((event2.getCode().equals(KeyCode.DIGIT1)) || (event2.getCode().equals(KeyCode.NUMPAD1))) {
                        changeCell(1, cellIndex);
                    } else if ((event2.getCode().equals(KeyCode.DIGIT2)) || (event2.getCode().equals(KeyCode.NUMPAD2))) {
                        changeCell(2, cellIndex);
                    } else if ((event2.getCode().equals(KeyCode.DIGIT3)) || (event2.getCode().equals(KeyCode.NUMPAD3))) {
                        changeCell(3, cellIndex);
                    } else if ((event2.getCode().equals(KeyCode.DIGIT4)) || (event2.getCode().equals(KeyCode.NUMPAD4))) {
                        changeCell(4, cellIndex);
                    } else if ((event2.getCode().equals(KeyCode.DIGIT5)) || (event2.getCode().equals(KeyCode.NUMPAD5))) {
                        changeCell(5, cellIndex);
                    } else if ((event2.getCode().equals(KeyCode.DIGIT6)) || (event2.getCode().equals(KeyCode.NUMPAD6))) {
                        changeCell(6, cellIndex);
                    } else if ((event2.getCode().equals(KeyCode.DIGIT7)) || (event2.getCode().equals(KeyCode.NUMPAD7))) {
                        changeCell(7, cellIndex);
                    } else if ((event2.getCode().equals(KeyCode.DIGIT8)) || (event2.getCode().equals(KeyCode.NUMPAD8))) {
                        changeCell(8, cellIndex);
                    } else if ((event2.getCode().equals(KeyCode.DIGIT9)) || (event2.getCode().equals(KeyCode.NUMPAD9))) {
                        changeCell(9, cellIndex);
                    } else if ((event2.getCode().equals(KeyCode.BACK_SPACE)) || (event2.getCode().equals(KeyCode.DIGIT0)) ||
                            (event2.getCode().equals(KeyCode.NUMPAD0))) {
                        changeCell(0, cellIndex);
                    }
                }
            });
        }
    }

    public void testWonGame() {
        for (int i = 1; i < 81; i++) {
            sudoku.currentKey.put(i, sudoku.answerKey.get(i));
        }
    }

    @FXML
    public void drawGrid() {
//        testWonGame();
        gameGrid.getChildren().clear();
        double cellWidth = gameGrid.getWidth() / sudoku.getLength2();
        double cellHeight = gameGrid.getHeight() / sudoku.getLength2();
        int count = 1;
        for (int i = 0; i < sudoku.getLength1(); i++) {
            for (int n = 0; n < sudoku.getLength2(); n++) {
                Rectangle cell = new Rectangle(n * cellWidth, i * cellHeight, cellWidth, cellHeight);
                cell.setFill(Color.WHITE);
                cell.setStroke(Color.BLACK);
                gameGrid.getChildren().add(cell);
                Label label = new Label("");
                setLabel(label, count);
                cellMap.put(count, cell);
                if (count == 81) {
                    labelChanger();
                }
                int cellIndex = sudoku.grid[n][i];
                if (showErrors) {
                    highlightWrongCells();
                }
                cell.setOnMouseClicked(event -> gridClicked(cellIndex));
                label.setOnMouseClicked(event -> gridClicked(cellIndex));
                count++;
            }
        }
    }
}