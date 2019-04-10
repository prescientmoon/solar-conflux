import { Component, OnInit } from '@angular/core';
import { AuthService } from 'src/app/core/services/auth.service';

interface MenuRoute{
  path:string;
  icon:string;
  name:string;
  description?:string;
}

@Component({
  selector: 'app-menu',
  templateUrl: './menu.component.html',
  styleUrls: ['./menu.component.scss']
})
export class MenuComponent implements OnInit {

  menuRoutes:MenuRoute[] = [{
    path:"account",
    icon:"person",
    name:"account",
    description:"all about you"
  },{
    path:"logs",
    icon:"library_books",
    name:"logs",
    description:"server logs"
  }]

  constructor( public auth:AuthService ) { }

  ngOnInit() {
  }

}
