import {Component, Input} from '@angular/core';
import {NgClass} from "@angular/common";

@Component({
  selector: 'app-game-cell',
  standalone: true,
  imports: [
    NgClass
  ],
  templateUrl: './game-cell.component.html',
  styleUrl: './game-cell.component.scss'
})
export class GameCellComponent {
  @Input() type!: string;
}
