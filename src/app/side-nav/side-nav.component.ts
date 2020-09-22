import { Component, OnInit, Inject } from '@angular/core';

@Component({
  selector: 'app-side-nav',
  templateUrl: './side-nav.component.html',
  styleUrls: ['./side-nav.component.css']
})

export class SideNavComponent {

  public navBarClass = "";

  initNav() {
    this.navBarClass = "navbar-nav";
    console.log(this.fillerNav);
  }

  constructor() {
    this.initNav();
  }

  doSomething(inputobj) {
    inputobj.obj.isFavourite = !inputobj.obj.isFavourite;
    if (inputobj.obj.isFavourite) {
      this.selectedItems.push(inputobj)
    } else {
      this.selectedItems.splice(this.selectedItems.indexOf(inputobj), 1);
    }
  }
  selectedItems = [];
  fillerNav = Array.from({ length: Math.floor(Math.random() * 10) + 1 }, (_, i) => new navComp("Nav component " + i));


  fillerContent = Array.from({ length: 50 }, () =>
    `Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut
       labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco
       laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in
       voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat
       cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum.`);

}

class navComp {
  public obj;

  constructor(@Inject(String) label: string) {

    this.obj = {
      "label": label,
      "isFavourite": false
    }
    this.obj.array = Array.from({ length: Math.floor(Math.random() * 6) + 1 }, (_, i) => this.obj.label + " child " + (i + 1))
  }

}

