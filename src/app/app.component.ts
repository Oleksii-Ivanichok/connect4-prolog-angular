import { Component } from '@angular/core';
import { CommonModule } from '@angular/common';
import { RouterOutlet } from '@angular/router';
import {GameCellComponent} from "./game-cell/game-cell.component";
import {HttpClient} from "@angular/common/http";
import {
  MatCell,
  MatCellDef,
  MatColumnDef,
  MatHeaderCell,
  MatHeaderCellDef,
  MatRow,
  MatRowDef,
  MatTable
} from "@angular/material/table";
import {MatButton} from "@angular/material/button";

@Component({
  selector: 'app-root',
  standalone: true,
  imports: [CommonModule, RouterOutlet, GameCellComponent, MatTable, MatRow, MatCell, MatColumnDef, MatCellDef, MatRowDef, MatHeaderCell, MatHeaderCellDef, MatButton],
  templateUrl: './app.component.html',
  styleUrl: './app.component.scss'
})
export class AppComponent {
  private baseUrl = 'http://localhost:8080';
  public rows:any;
  public cols:any;
  public depth:any;
  public board: string[][];
  displayedColumns: string[];
  gameField: string[][];
  public gameStatus: any;
  constructor(private http: HttpClient) {
    this.rows = prompt("Enter the number of rows", "7");
    this.cols = prompt("Enter the number of column", "6");
    this.depth = prompt("Enter depth of search (1-5)", "1");
    this.http.post(this.baseUrl + '/start-game',
      {
        width: parseInt(this.rows),
        height: parseInt(this.cols),
        depth: parseInt(this.depth)
      }
    ).subscribe(res => {
      console.log(res);
    });
    this.board = this.createBoard(this.rows, this.cols);
    console.log(this.board);
    this.gameField = this.createBoard(this.rows, this.cols);
    this.displayedColumns = Array.from({ length: this.cols }, (_, i) => `${i}Column `);

  }

  createBoard(rows: number, cols: number) {
    let array: string[][] = [];
    for (let i = 0; i < rows; i++) {
      array.push([]);
      for (let j = 0; j < cols; j++) {
        array[i].push("unactivated");
      }
    }
    return array;
  }

  onColumnClick(columnName: string): void {
    const columnIndex = parseInt(columnName);
    for (let i = this.gameField.length - 1; i >= 0; i--) {
      if (this.gameField[i][columnIndex] === "unactivated") {
        this.gameField[i][columnIndex] = 'player';
        break;
      }
    }
    this.gameStatus = this.checkWin(this.gameField);
    console.log(this.checkWin(this.gameField));
    const data = {column: columnIndex + 1}
    this.http.post(this.baseUrl + '/selected', data)
      .subscribe((res:any) => {
        this.computerMove(res.column - 1)
      console.log(res);
    });

  }

  computerMove(column: number): void {
    for (let i = this.gameField.length - 1; i >= 0; i--) {
      if (this.gameField[i][column] === "unactivated") {
        this.gameField[i][column] = 'computer';
        break;
      }
    }
    this.gameStatus = this.checkWin(this.gameField);
    console.log(this.checkWin(this.gameField));
    console.log(this.gameField)
  }

  checkWin(matrix: string[][]){
    const rows = matrix.length;
    const cols = matrix[0].length;

    // Check horizontally
    for (let i = 0; i < rows; i++) {
      for (let j = 0; j <= cols - 4; j++) {
        if (matrix[i][j] === matrix[i][j + 1] &&
          matrix[i][j] === matrix[i][j + 2] &&
          matrix[i][j] === matrix[i][j + 3] &&
          matrix[i][j] !== "unactivated") {
          setTimeout(() => {
            alert(matrix[i][j] + ' wins');
            location.reload();
          }, 1000);
        }
      }
    }

    // Check vertically
    for (let j = 0; j < cols; j++) {
      for (let i = 0; i <= rows - 4; i++) {
        if (matrix[i][j] === matrix[i + 1][j] &&
          matrix[i][j] === matrix[i + 2][j] &&
          matrix[i][j] === matrix[i + 3][j] &&
          matrix[i][j] !== "unactivated") {
          setTimeout(() => {
            alert(matrix[i][j] + ' wins');
            location.reload();
          }, 1000);
        }
      }
    }

    // Check diagonally (from top-left to bottom-right)
    for (let i = 0; i <= rows - 4; i++) {
      for (let j = 0; j <= cols - 4; j++) {
        if (matrix[i][j] === matrix[i + 1][j + 1] &&
          matrix[i][j] === matrix[i + 2][j + 2] &&
          matrix[i][j] === matrix[i + 3][j + 3] &&
          matrix[i][j] !== "unactivated") {
          setTimeout(() => {
            alert(matrix[i][j] + ' wins');
            location.reload();
          }, 1000);
        }
      }
    }

    // Check diagonally (from top-right to bottom-left)
    for (let i = 0; i <= rows - 4; i++) {
      for (let j = cols - 1; j >= 3; j--) {
        if (matrix[i][j] === matrix[i + 1][j - 1] &&
          matrix[i][j] === matrix[i + 2][j - 2] &&
          matrix[i][j] === matrix[i + 3][j - 3] &&
          matrix[i][j] !== "unactivated") {
          setTimeout(() => {
            alert(matrix[i][j] + ' wins');
            location.reload();
          }, 1000);
        }
      }
    }
    let unactivatedCount = 0;
    // Count unactivated cells
    for (let i = 0; i < rows; i++) {
      for (let j = 0; j < cols; j++) {
        if (matrix[i][j] === "unactivated") {
          unactivatedCount++;
        }
      }
    }

    if (unactivatedCount === 0) {
      setTimeout(() => {
        alert('draw');
        location.reload();
      }, 1000);
    }

    return false;
  }


  protected readonly location = location;
}
