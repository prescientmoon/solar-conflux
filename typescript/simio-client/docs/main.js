(window["webpackJsonp"] = window["webpackJsonp"] || []).push([["main"],{

/***/ "./src/$$_lazy_route_resource lazy recursive":
/*!**********************************************************!*\
  !*** ./src/$$_lazy_route_resource lazy namespace object ***!
  \**********************************************************/
/*! no static exports found */
/***/ (function(module, exports) {

function webpackEmptyAsyncContext(req) {
	// Here Promise.resolve().then() is used instead of new Promise() to prevent
	// uncaught exception popping up in devtools
	return Promise.resolve().then(function() {
		var e = new Error("Cannot find module '" + req + "'");
		e.code = 'MODULE_NOT_FOUND';
		throw e;
	});
}
webpackEmptyAsyncContext.keys = function() { return []; };
webpackEmptyAsyncContext.resolve = webpackEmptyAsyncContext;
module.exports = webpackEmptyAsyncContext;
webpackEmptyAsyncContext.id = "./src/$$_lazy_route_resource lazy recursive";

/***/ }),

/***/ "./src/app/app-routing.module.ts":
/*!***************************************!*\
  !*** ./src/app/app-routing.module.ts ***!
  \***************************************/
/*! exports provided: AppRoutingModule */
/***/ (function(module, __webpack_exports__, __webpack_require__) {

"use strict";
__webpack_require__.r(__webpack_exports__);
/* harmony export (binding) */ __webpack_require__.d(__webpack_exports__, "AppRoutingModule", function() { return AppRoutingModule; });
/* harmony import */ var tslib__WEBPACK_IMPORTED_MODULE_0__ = __webpack_require__(/*! tslib */ "./node_modules/tslib/tslib.es6.js");
/* harmony import */ var _angular_core__WEBPACK_IMPORTED_MODULE_1__ = __webpack_require__(/*! @angular/core */ "./node_modules/@angular/core/fesm5/core.js");
/* harmony import */ var _angular_router__WEBPACK_IMPORTED_MODULE_2__ = __webpack_require__(/*! @angular/router */ "./node_modules/@angular/router/fesm5/router.js");
/* harmony import */ var _components_logging_logs_logs_component__WEBPACK_IMPORTED_MODULE_3__ = __webpack_require__(/*! ./components/logging/logs/logs.component */ "./src/app/components/logging/logs/logs.component.ts");
/* harmony import */ var _components_auth_login_login_component__WEBPACK_IMPORTED_MODULE_4__ = __webpack_require__(/*! ./components/auth/login/login.component */ "./src/app/components/auth/login/login.component.ts");
/* harmony import */ var _components_account_account_account_component__WEBPACK_IMPORTED_MODULE_5__ = __webpack_require__(/*! ./components/account/account/account.component */ "./src/app/components/account/account/account.component.ts");
/* harmony import */ var _core_guards_auth_guard_guard__WEBPACK_IMPORTED_MODULE_6__ = __webpack_require__(/*! ./core/guards/auth-guard.guard */ "./src/app/core/guards/auth-guard.guard.ts");
/* harmony import */ var _components_account_users_users_component__WEBPACK_IMPORTED_MODULE_7__ = __webpack_require__(/*! ./components/account/users/users.component */ "./src/app/components/account/users/users.component.ts");
/* harmony import */ var _components_account_user_user_component__WEBPACK_IMPORTED_MODULE_8__ = __webpack_require__(/*! ./components/account/user/user.component */ "./src/app/components/account/user/user.component.ts");









var routes = [{
        path: "logs",
        component: _components_logging_logs_logs_component__WEBPACK_IMPORTED_MODULE_3__["LogsComponent"]
    }, {
        path: "login",
        component: _components_auth_login_login_component__WEBPACK_IMPORTED_MODULE_4__["LoginComponent"]
    }, {
        path: "account",
        component: _components_account_account_account_component__WEBPACK_IMPORTED_MODULE_5__["AccountComponent"],
        canActivate: [_core_guards_auth_guard_guard__WEBPACK_IMPORTED_MODULE_6__["AuthGuard"]]
    }, {
        path: "users",
        component: _components_account_users_users_component__WEBPACK_IMPORTED_MODULE_7__["UsersComponent"]
    }, {
        path: "user/:uid",
        component: _components_account_user_user_component__WEBPACK_IMPORTED_MODULE_8__["UserComponent"]
    }];
var AppRoutingModule = /** @class */ (function () {
    function AppRoutingModule() {
    }
    AppRoutingModule = tslib__WEBPACK_IMPORTED_MODULE_0__["__decorate"]([
        Object(_angular_core__WEBPACK_IMPORTED_MODULE_1__["NgModule"])({
            imports: [_angular_router__WEBPACK_IMPORTED_MODULE_2__["RouterModule"].forRoot(routes)],
            exports: [_angular_router__WEBPACK_IMPORTED_MODULE_2__["RouterModule"]]
        })
    ], AppRoutingModule);
    return AppRoutingModule;
}());



/***/ }),

/***/ "./src/app/app.component.html":
/*!************************************!*\
  !*** ./src/app/app.component.html ***!
  \************************************/
/*! no static exports found */
/***/ (function(module, exports) {

module.exports = "<div class=\"main\">\n    <mat-sidenav-container class=\"main-menu\">\n        <mat-sidenav mode=\"side\" #sidenav opened class=\"menu-sidebar\">\n            <app-menu></app-menu>\n        </mat-sidenav>\n\n        <mat-sidenav-content>\n            <!-- <button class=\"menu-handler\" mat-button (click)=\"sidenav.toggle()\">toggle</button>\n            {{sidenav.opened}} -->\n            <router-outlet></router-outlet>\n        </mat-sidenav-content>\n    </mat-sidenav-container>\n</div>"

/***/ }),

/***/ "./src/app/app.component.scss":
/*!************************************!*\
  !*** ./src/app/app.component.scss ***!
  \************************************/
/*! no static exports found */
/***/ (function(module, exports) {

module.exports = ".main {\n  height: 100%; }\n\n.main-menu {\n  height: 100%; }\n\n.main-menu .menu-sidebar {\n    width: 10%; }\n\n@media only screen and (max-width: 1200px) {\n  .main-menu .menu-sidebar {\n    width: 15%; } }\n\n@media only screen and (max-width: 1000px) {\n  .main-menu .menu-sidebar {\n    width: 20%; } }\n\n@media only screen and (max-width: 720px) {\n  .main-menu .menu-sidebar {\n    width: 67px; } }\n\n/*# sourceMappingURL=data:application/json;base64,eyJ2ZXJzaW9uIjozLCJzb3VyY2VzIjpbInNyYy9hcHAvYzpcXFVzZXJzXFxsYXJ5bVxccmVwb3NcXGFuZ3VsYXItY2xpZW50L3NyY1xcYXBwXFxhcHAuY29tcG9uZW50LnNjc3MiXSwibmFtZXMiOltdLCJtYXBwaW5ncyI6IkFBQUE7RUFDSSxZQUFXLEVBQUE7O0FBRWY7RUFDSSxZQUFXLEVBQUE7O0FBRGY7SUFHUSxVQUFTLEVBQUE7O0FBSWpCO0VBQ0k7SUFFUSxVQUFVLEVBQUEsRUFDYjs7QUFLVDtFQUNJO0lBRVEsVUFBVSxFQUFBLEVBQ2I7O0FBS1Q7RUFDSTtJQUVRLFdBQVUsRUFBQSxFQUNiIiwiZmlsZSI6InNyYy9hcHAvYXBwLmNvbXBvbmVudC5zY3NzIiwic291cmNlc0NvbnRlbnQiOlsiLm1haW57XHJcbiAgICBoZWlnaHQ6MTAwJTtcclxufVxyXG4ubWFpbi1tZW51e1xyXG4gICAgaGVpZ2h0OjEwMCU7XHJcbiAgICAubWVudS1zaWRlYmFye1xyXG4gICAgICAgIHdpZHRoOjEwJTtcclxuICAgIH1cclxufVxyXG5cclxuQG1lZGlhIG9ubHkgc2NyZWVuIGFuZCAobWF4LXdpZHRoOiAxMjAwcHgpIHtcclxuICAgIC5tYWluLW1lbnUge1xyXG4gICAgICAgIC5tZW51LXNpZGViYXIge1xyXG4gICAgICAgICAgICB3aWR0aDogMTUlO1xyXG4gICAgICAgIH1cclxuICAgIH1cclxufVxyXG5cclxuXHJcbkBtZWRpYSBvbmx5IHNjcmVlbiBhbmQgKG1heC13aWR0aDogMTAwMHB4KSB7XHJcbiAgICAubWFpbi1tZW51IHtcclxuICAgICAgICAubWVudS1zaWRlYmFyIHtcclxuICAgICAgICAgICAgd2lkdGg6IDIwJTtcclxuICAgICAgICB9XHJcbiAgICB9XHJcbn1cclxuXHJcblxyXG5AbWVkaWEgb25seSBzY3JlZW4gYW5kIChtYXgtd2lkdGg6NzIwcHgpIHtcclxuICAgIC5tYWluLW1lbnV7XHJcbiAgICAgICAgLm1lbnUtc2lkZWJhcntcclxuICAgICAgICAgICAgd2lkdGg6NjdweDtcclxuICAgICAgICB9XHJcbiAgICB9ICAgXHJcbn1cclxuXHJcblxyXG5cclxuLy8gQG1lZGlhIG9ubHkgc2NyZWVuIGFuZCAobWF4LXdpZHRoOiA3MjBweCkge1xyXG4vLyAgICAgLm1haW4tbWVudSB7XHJcbi8vICAgICAgICAgLm1lbnUtc2lkZWJhciB7XHJcbi8vICAgICAgICAgICAgIHdpZHRoOiAyMCU7XHJcbi8vICAgICAgICAgfVxyXG4vLyAgICAgfVxyXG4vLyB9Il19 */"

/***/ }),

/***/ "./src/app/app.component.ts":
/*!**********************************!*\
  !*** ./src/app/app.component.ts ***!
  \**********************************/
/*! exports provided: AppComponent */
/***/ (function(module, __webpack_exports__, __webpack_require__) {

"use strict";
__webpack_require__.r(__webpack_exports__);
/* harmony export (binding) */ __webpack_require__.d(__webpack_exports__, "AppComponent", function() { return AppComponent; });
/* harmony import */ var tslib__WEBPACK_IMPORTED_MODULE_0__ = __webpack_require__(/*! tslib */ "./node_modules/tslib/tslib.es6.js");
/* harmony import */ var _angular_core__WEBPACK_IMPORTED_MODULE_1__ = __webpack_require__(/*! @angular/core */ "./node_modules/@angular/core/fesm5/core.js");


var AppComponent = /** @class */ (function () {
    function AppComponent() {
        this.title = 'angular-client';
    }
    AppComponent = tslib__WEBPACK_IMPORTED_MODULE_0__["__decorate"]([
        Object(_angular_core__WEBPACK_IMPORTED_MODULE_1__["Component"])({
            selector: 'app-root',
            template: __webpack_require__(/*! ./app.component.html */ "./src/app/app.component.html"),
            styles: [__webpack_require__(/*! ./app.component.scss */ "./src/app/app.component.scss")]
        })
    ], AppComponent);
    return AppComponent;
}());



/***/ }),

/***/ "./src/app/app.module.ts":
/*!*******************************!*\
  !*** ./src/app/app.module.ts ***!
  \*******************************/
/*! exports provided: AppModule */
/***/ (function(module, __webpack_exports__, __webpack_require__) {

"use strict";
__webpack_require__.r(__webpack_exports__);
/* harmony export (binding) */ __webpack_require__.d(__webpack_exports__, "AppModule", function() { return AppModule; });
/* harmony import */ var tslib__WEBPACK_IMPORTED_MODULE_0__ = __webpack_require__(/*! tslib */ "./node_modules/tslib/tslib.es6.js");
/* harmony import */ var _angular_platform_browser__WEBPACK_IMPORTED_MODULE_1__ = __webpack_require__(/*! @angular/platform-browser */ "./node_modules/@angular/platform-browser/fesm5/platform-browser.js");
/* harmony import */ var _angular_core__WEBPACK_IMPORTED_MODULE_2__ = __webpack_require__(/*! @angular/core */ "./node_modules/@angular/core/fesm5/core.js");
/* harmony import */ var _app_routing_module__WEBPACK_IMPORTED_MODULE_3__ = __webpack_require__(/*! ./app-routing.module */ "./src/app/app-routing.module.ts");
/* harmony import */ var _app_component__WEBPACK_IMPORTED_MODULE_4__ = __webpack_require__(/*! ./app.component */ "./src/app/app.component.ts");
/* harmony import */ var _components_logging_logs_logs_component__WEBPACK_IMPORTED_MODULE_5__ = __webpack_require__(/*! ./components/logging/logs/logs.component */ "./src/app/components/logging/logs/logs.component.ts");
/* harmony import */ var _components_logging_log_log_component__WEBPACK_IMPORTED_MODULE_6__ = __webpack_require__(/*! ./components/logging/log/log.component */ "./src/app/components/logging/log/log.component.ts");
/* harmony import */ var _angular_common_http__WEBPACK_IMPORTED_MODULE_7__ = __webpack_require__(/*! @angular/common/http */ "./node_modules/@angular/common/fesm5/http.js");
/* harmony import */ var _angular_platform_browser_animations__WEBPACK_IMPORTED_MODULE_8__ = __webpack_require__(/*! @angular/platform-browser/animations */ "./node_modules/@angular/platform-browser/fesm5/animations.js");
/* harmony import */ var _core_core_module__WEBPACK_IMPORTED_MODULE_9__ = __webpack_require__(/*! ./core/core.module */ "./src/app/core/core.module.ts");
/* harmony import */ var _components_auth_login_login_component__WEBPACK_IMPORTED_MODULE_10__ = __webpack_require__(/*! ./components/auth/login/login.component */ "./src/app/components/auth/login/login.component.ts");
/* harmony import */ var _angular_fire__WEBPACK_IMPORTED_MODULE_11__ = __webpack_require__(/*! @angular/fire */ "./node_modules/@angular/fire/index.js");
/* harmony import */ var src_environments_environment__WEBPACK_IMPORTED_MODULE_12__ = __webpack_require__(/*! src/environments/environment */ "./src/environments/environment.ts");
/* harmony import */ var _components_menu_menu_component__WEBPACK_IMPORTED_MODULE_13__ = __webpack_require__(/*! ./components/menu/menu.component */ "./src/app/components/menu/menu.component.ts");














var AppModule = /** @class */ (function () {
    function AppModule() {
    }
    AppModule = tslib__WEBPACK_IMPORTED_MODULE_0__["__decorate"]([
        Object(_angular_core__WEBPACK_IMPORTED_MODULE_2__["NgModule"])({
            declarations: [
                _app_component__WEBPACK_IMPORTED_MODULE_4__["AppComponent"],
                _components_logging_logs_logs_component__WEBPACK_IMPORTED_MODULE_5__["LogsComponent"],
                _components_logging_log_log_component__WEBPACK_IMPORTED_MODULE_6__["LogComponent"],
                _components_auth_login_login_component__WEBPACK_IMPORTED_MODULE_10__["LoginComponent"],
                _components_menu_menu_component__WEBPACK_IMPORTED_MODULE_13__["MenuComponent"]
            ],
            imports: [
                _angular_common_http__WEBPACK_IMPORTED_MODULE_7__["HttpClientModule"],
                _angular_platform_browser__WEBPACK_IMPORTED_MODULE_1__["BrowserModule"],
                _angular_platform_browser_animations__WEBPACK_IMPORTED_MODULE_8__["BrowserAnimationsModule"],
                _core_core_module__WEBPACK_IMPORTED_MODULE_9__["CoreModule"],
                _angular_fire__WEBPACK_IMPORTED_MODULE_11__["AngularFireModule"].initializeApp(src_environments_environment__WEBPACK_IMPORTED_MODULE_12__["environment"].firebase),
                _app_routing_module__WEBPACK_IMPORTED_MODULE_3__["AppRoutingModule"]
            ],
            providers: [],
            bootstrap: [_app_component__WEBPACK_IMPORTED_MODULE_4__["AppComponent"]]
        })
    ], AppModule);
    return AppModule;
}());



/***/ }),

/***/ "./src/app/components/account/account/account.component.html":
/*!*******************************************************************!*\
  !*** ./src/app/components/account/account/account.component.html ***!
  \*******************************************************************/
/*! no static exports found */
/***/ (function(module, exports) {

module.exports = "<div *ngIf=\"auth.user | async as user\" class=\"account-container\">\n  <mat-card>\n    <mat-card-header>\n      <mat-card-title>\n        {{user.displayName}}\n      </mat-card-title>\n      <mat-card-subtitle>\n        {{user.email}}\n      </mat-card-subtitle>\n      <div mat-card-avatar class=avatar [style.background-image]=\"'url(' + user.photoUrl + ')'\"> </div>\n    </mat-card-header>\n    <mat-card-content>\n      <mat-card>\n        <mat-card-header>\n          <mat-card-title>Description</mat-card-title>\n          <mat-card-subtitle>this is supposed to describe you</mat-card-subtitle>\n          <div mat-card-avatar>\n            <button mat-mini-fab>\n              <div *ngIf=\"!editing; else save\">\n                <mat-icon (click)=\"startEditing()\">\n                  edit\n                </mat-icon>\n              </div>\n            </button>\n          </div>\n        </mat-card-header>\n        <mat-card-content>\n          <div *ngIf=\"editing; else notEditing\">\n            <form>\n              <mat-form-field>\n                <textarea #descriptionInput cdkTextareaAutosize cdkAutosizeMinRows=3 cols=30 maxlength=189\n                  [cdkAutosizeMaxRows]=maxDescriptionRows matInput placeholder=description\n                  [value]=\"user.description || defaultDescription(user.displayName)\"></textarea>\n              </mat-form-field>\n            </form>\n          </div>\n        </mat-card-content>\n      </mat-card>\n      <mat-list>\n        <div *ngFor=\"let uid of (friends.uids)\">\n          <app-public-account [mode]=\"'card'\" [uid]=\"uid\" [allowMenu]=\"true\" [menu]=\"publicAccountMenu\">\n          </app-public-account>\n\n          <mat-menu #publicAccountMenu=\"matMenu\">\n            <button mat-menu-item (click)=\"friends.removeFriend(uid)\">\n              <mat-icon>person_add_disabled</mat-icon> Remove friend\n            </button>\n            <button mat-menu-item [routerLink]=\"[ '/user', uid ]\" routerLinkActive=\"active\">\n              <mat-icon>account_circle</mat-icon>Profile\n            </button>\n            <div *ngIf=\"canCopy()\">\n              <button mat-menu-item (click)=\"copyToClipboard(uid)\">\n                <mat-icon>file_copy</mat-icon> Copy url \n              </button>\n            </div>\n          </mat-menu>\n        </div>\n      </mat-list>\n    </mat-card-content>\n  </mat-card>\n</div>\n\n\n<ng-template #save>\n  <div *ngIf=\"auth.user | async as user\">\n    <mat-icon (click)=\"saveDescription(user.uid)\">\n      save\n    </mat-icon>\n  </div>\n</ng-template>\n\n<ng-template #notEditing>\n  <div *ngIf=\"auth.user | async as user\">\n    <div *ngIf=\"user.description; else withoutDesc\">\n      {{user.description}}\n    </div>\n  </div>\n</ng-template>\n\n<ng-template #withoutDesc>\n  <div class=\"withoutDescContainer\">\n    <div class=\"noDescInfo\">\n      You dont have a description:\n    </div>\n    <button (click)=\"startEditing()\" color=primary mat-raised-button>\n      Create your description\n    </button>\n  </div>\n</ng-template>"

/***/ }),

/***/ "./src/app/components/account/account/account.component.scss":
/*!*******************************************************************!*\
  !*** ./src/app/components/account/account/account.component.scss ***!
  \*******************************************************************/
/*! no static exports found */
/***/ (function(module, exports) {

module.exports = ".withoutDescContainer {\n  text-align: center;\n  display: flex;\n  justify-content: center;\n  align-items: center;\n  flex-direction: column; }\n\n.noDescInfo {\n  color: rgba(255, 255, 255, 0.5);\n  font-size: 75%;\n  margin: 3%; }\n\n.account-container {\n  overflow-x: hidden; }\n\n/*# sourceMappingURL=data:application/json;base64,eyJ2ZXJzaW9uIjozLCJzb3VyY2VzIjpbInNyYy9hcHAvY29tcG9uZW50cy9hY2NvdW50L2FjY291bnQvYzpcXFVzZXJzXFxsYXJ5bVxccmVwb3NcXGFuZ3VsYXItY2xpZW50L3NyY1xcYXBwXFxjb21wb25lbnRzXFxhY2NvdW50XFxhY2NvdW50XFxhY2NvdW50LmNvbXBvbmVudC5zY3NzIl0sIm5hbWVzIjpbXSwibWFwcGluZ3MiOiJBQUFBO0VBQ0ksa0JBQWtCO0VBRWxCLGFBQWE7RUFDYix1QkFBdUI7RUFDdkIsbUJBQW1CO0VBQ25CLHNCQUFzQixFQUFBOztBQUUxQjtFQUNJLCtCQUEyQjtFQUMzQixjQUFjO0VBQ2QsVUFBUyxFQUFBOztBQUViO0VBQ0ksa0JBQWtCLEVBQUEiLCJmaWxlIjoic3JjL2FwcC9jb21wb25lbnRzL2FjY291bnQvYWNjb3VudC9hY2NvdW50LmNvbXBvbmVudC5zY3NzIiwic291cmNlc0NvbnRlbnQiOlsiLndpdGhvdXREZXNjQ29udGFpbmVye1xyXG4gICAgdGV4dC1hbGlnbjogY2VudGVyO1xyXG4gICAgLy8gbWFyZ2luOiAxNSU7XHJcbiAgICBkaXNwbGF5OiBmbGV4O1xyXG4gICAganVzdGlmeS1jb250ZW50OiBjZW50ZXI7XHJcbiAgICBhbGlnbi1pdGVtczogY2VudGVyO1xyXG4gICAgZmxleC1kaXJlY3Rpb246IGNvbHVtbjtcclxufVxyXG4ubm9EZXNjSW5mb3tcclxuICAgIGNvbG9yOnJnYmEoMjU2LDI1NiwyNTYsMC41KTtcclxuICAgIGZvbnQtc2l6ZTogNzUlO1xyXG4gICAgbWFyZ2luOjMlO1xyXG59XHJcbi5hY2NvdW50LWNvbnRhaW5lcntcclxuICAgIG92ZXJmbG93LXg6IGhpZGRlbjtcclxufSJdfQ== */"

/***/ }),

/***/ "./src/app/components/account/account/account.component.ts":
/*!*****************************************************************!*\
  !*** ./src/app/components/account/account/account.component.ts ***!
  \*****************************************************************/
/*! exports provided: AccountComponent */
/***/ (function(module, __webpack_exports__, __webpack_require__) {

"use strict";
__webpack_require__.r(__webpack_exports__);
/* harmony export (binding) */ __webpack_require__.d(__webpack_exports__, "AccountComponent", function() { return AccountComponent; });
/* harmony import */ var tslib__WEBPACK_IMPORTED_MODULE_0__ = __webpack_require__(/*! tslib */ "./node_modules/tslib/tslib.es6.js");
/* harmony import */ var _angular_core__WEBPACK_IMPORTED_MODULE_1__ = __webpack_require__(/*! @angular/core */ "./node_modules/@angular/core/fesm5/core.js");
/* harmony import */ var src_app_core_services_auth_service__WEBPACK_IMPORTED_MODULE_2__ = __webpack_require__(/*! src/app/core/services/auth.service */ "./src/app/core/services/auth.service.ts");
/* harmony import */ var rxjs_operators__WEBPACK_IMPORTED_MODULE_3__ = __webpack_require__(/*! rxjs/operators */ "./node_modules/rxjs/_esm5/operators/index.js");
/* harmony import */ var _angular_router__WEBPACK_IMPORTED_MODULE_4__ = __webpack_require__(/*! @angular/router */ "./node_modules/@angular/router/fesm5/router.js");
/* harmony import */ var src_app_core_services_friend_system_service__WEBPACK_IMPORTED_MODULE_5__ = __webpack_require__(/*! src/app/core/services/friend-system.service */ "./src/app/core/services/friend-system.service.ts");






var AccountComponent = /** @class */ (function () {
    /**
     * the component used to view and edit your account
     * @param auth the auth service (injected)
     * @param friends the friends service (injected)
     * @param router the router service (injected)
     */
    function AccountComponent(auth, friends, router) {
        this.auth = auth;
        this.friends = friends;
        this.router = router;
        /**
         * a limit for how much the description textarea should resize
         */
        this.maxDescriptionRows = 7;
        /**
         * a flag indicating the state of the description editor
         */
        this.editing = false;
    }
    /**
     * runs when the component is inited
     */
    AccountComponent.prototype.ngOnInit = function () {
        var _this = this;
        //be sure to only show this if we are logged in
        this.auth.user.pipe(Object(rxjs_operators__WEBPACK_IMPORTED_MODULE_3__["filter"])(function (value) { return !value; } //only continues if the user is null
        )).subscribe(function (value) { return _this.router.navigate(["/login"]); } //reddirect to login
        );
    };
    /**
     * sets the editing flag to true
     */
    AccountComponent.prototype.startEditing = function () {
        this.editing = true;
    };
    /**
     * used to update the description of an user
     * @param uid the uid of the user
     */
    AccountComponent.prototype.saveDescription = function (uid) {
        //disable the editing flag
        this.editing = false;
        //save the description into firesotre
        this.auth.updateUser(uid, {
            description: this.currentDescription.nativeElement.value
        });
    };
    /**
     * used to get the default description for new users,
     * beacuse template literals arent avabile inside html
     * @param name the name to include in the description
     * @returns the default description for new users
     */
    AccountComponent.prototype.defaultDescription = function (name) {
        return "Hi! My name is " + name;
    };
    /**
     * used to test the avability of the clipboard api
     * @returns either true or false, representing the avability of the clipboard API
     */
    AccountComponent.prototype.canCopy = function () {
        //@ts-ignore
        return !!navigator.clipboard;
    };
    /**
     * used to copy the url of an user to clipboard
     * @param uid the uid of the user to copy link to keyboard
     */
    AccountComponent.prototype.copyToClipboard = function (uid) {
        //@ts-ignore
        navigator.clipboard.writeText(window.location.href + "/user/" + uid);
    };
    tslib__WEBPACK_IMPORTED_MODULE_0__["__decorate"]([
        Object(_angular_core__WEBPACK_IMPORTED_MODULE_1__["ViewChild"])("descriptionInput"),
        tslib__WEBPACK_IMPORTED_MODULE_0__["__metadata"]("design:type", _angular_core__WEBPACK_IMPORTED_MODULE_1__["ElementRef"])
    ], AccountComponent.prototype, "currentDescription", void 0);
    AccountComponent = tslib__WEBPACK_IMPORTED_MODULE_0__["__decorate"]([
        Object(_angular_core__WEBPACK_IMPORTED_MODULE_1__["Component"])({
            selector: 'app-account',
            template: __webpack_require__(/*! ./account.component.html */ "./src/app/components/account/account/account.component.html"),
            styles: [__webpack_require__(/*! ./account.component.scss */ "./src/app/components/account/account/account.component.scss")]
        }),
        tslib__WEBPACK_IMPORTED_MODULE_0__["__metadata"]("design:paramtypes", [src_app_core_services_auth_service__WEBPACK_IMPORTED_MODULE_2__["AuthService"],
            src_app_core_services_friend_system_service__WEBPACK_IMPORTED_MODULE_5__["FriendSystemService"],
            _angular_router__WEBPACK_IMPORTED_MODULE_4__["Router"]])
    ], AccountComponent);
    return AccountComponent;
}());



/***/ }),

/***/ "./src/app/components/account/public-account/public-account.component.html":
/*!*********************************************************************************!*\
  !*** ./src/app/components/account/public-account/public-account.component.html ***!
  \*********************************************************************************/
/*! no static exports found */
/***/ (function(module, exports) {

module.exports = "<div *ngIf=\"user | async as $user\">\n  <div *ngIf=\"mode == 'card'; then card else list\"></div>\n  <ng-template #card>\n    <mat-card>\n      <mat-card-header>\n        <mat-card-title>\n          {{$user.displayName}}\n        </mat-card-title>\n        <mat-card-subtitle>\n          <div *ngIf=\"$user.description; else emailFallback\">\n            {{$user.description}}\n          </div>\n        </mat-card-subtitle>\n        <div mat-card-avatar [matMenuTriggerFor]=\"menu\" class=avatar\n          [style.background-image]=\"'url(' + $user.photoUrl + ')'\">\n\n        </div>\n      </mat-card-header>\n    </mat-card>\n  </ng-template>\n\n  <ng-template #emailFallback>\n    {{$user.email}}\n  </ng-template>\n\n</div>\n\n<ng-template #noUidError>\n  Something went wrong...\n</ng-template>"

/***/ }),

/***/ "./src/app/components/account/public-account/public-account.component.scss":
/*!*********************************************************************************!*\
  !*** ./src/app/components/account/public-account/public-account.component.scss ***!
  \*********************************************************************************/
/*! no static exports found */
/***/ (function(module, exports) {

module.exports = "#list-avatar {\n  background-color: red; }\n\n/*# sourceMappingURL=data:application/json;base64,eyJ2ZXJzaW9uIjozLCJzb3VyY2VzIjpbInNyYy9hcHAvY29tcG9uZW50cy9hY2NvdW50L3B1YmxpYy1hY2NvdW50L2M6XFxVc2Vyc1xcbGFyeW1cXHJlcG9zXFxhbmd1bGFyLWNsaWVudC9zcmNcXGFwcFxcY29tcG9uZW50c1xcYWNjb3VudFxccHVibGljLWFjY291bnRcXHB1YmxpYy1hY2NvdW50LmNvbXBvbmVudC5zY3NzIl0sIm5hbWVzIjpbXSwibWFwcGluZ3MiOiJBQUFBO0VBRUkscUJBQXFCLEVBQUEiLCJmaWxlIjoic3JjL2FwcC9jb21wb25lbnRzL2FjY291bnQvcHVibGljLWFjY291bnQvcHVibGljLWFjY291bnQuY29tcG9uZW50LnNjc3MiLCJzb3VyY2VzQ29udGVudCI6WyIjbGlzdC1hdmF0YXJ7XHJcbiAgICAvLyBoZWlnaHQ6MTAwJTtcclxuICAgIGJhY2tncm91bmQtY29sb3I6IHJlZDtcclxuICAgIC8vIHdpZHRoOjEwMCU7XHJcbn0iXX0= */"

/***/ }),

/***/ "./src/app/components/account/public-account/public-account.component.ts":
/*!*******************************************************************************!*\
  !*** ./src/app/components/account/public-account/public-account.component.ts ***!
  \*******************************************************************************/
/*! exports provided: PublicAccountComponent */
/***/ (function(module, __webpack_exports__, __webpack_require__) {

"use strict";
__webpack_require__.r(__webpack_exports__);
/* harmony export (binding) */ __webpack_require__.d(__webpack_exports__, "PublicAccountComponent", function() { return PublicAccountComponent; });
/* harmony import */ var tslib__WEBPACK_IMPORTED_MODULE_0__ = __webpack_require__(/*! tslib */ "./node_modules/tslib/tslib.es6.js");
/* harmony import */ var _angular_core__WEBPACK_IMPORTED_MODULE_1__ = __webpack_require__(/*! @angular/core */ "./node_modules/@angular/core/fesm5/core.js");
/* harmony import */ var src_app_core_services_account_service__WEBPACK_IMPORTED_MODULE_2__ = __webpack_require__(/*! src/app/core/services/account.service */ "./src/app/core/services/account.service.ts");



var PublicAccountComponent = /** @class */ (function () {
    /**
     * used to display the data about an user
     * @param pas the public account servive (injected)
     */
    function PublicAccountComponent(pas) {
        this.pas = pas;
        /**
         * the mode the data is displayed
         * @default "card"
         */
        this.mode = "card";
        this.allowMenu = false;
    }
    /**
     * runs when the component is inited
     */
    PublicAccountComponent.prototype.ngOnInit = function () {
        //get user by uid
        if (this.uid)
            this.user = this.pas.getUser(this.uid);
        else
            throw new Error("cannot display user without knowing its uid");
        //throw error if template is allowed but not setted
        if (this.allowMenu && !this.menu)
            throw new Error("menu enabled but not asigned");
    };
    tslib__WEBPACK_IMPORTED_MODULE_0__["__decorate"]([
        Object(_angular_core__WEBPACK_IMPORTED_MODULE_1__["Input"])("uid"),
        tslib__WEBPACK_IMPORTED_MODULE_0__["__metadata"]("design:type", String)
    ], PublicAccountComponent.prototype, "uid", void 0);
    tslib__WEBPACK_IMPORTED_MODULE_0__["__decorate"]([
        Object(_angular_core__WEBPACK_IMPORTED_MODULE_1__["Input"])("mode"),
        tslib__WEBPACK_IMPORTED_MODULE_0__["__metadata"]("design:type", String)
    ], PublicAccountComponent.prototype, "mode", void 0);
    tslib__WEBPACK_IMPORTED_MODULE_0__["__decorate"]([
        Object(_angular_core__WEBPACK_IMPORTED_MODULE_1__["Input"])("allowMenu"),
        tslib__WEBPACK_IMPORTED_MODULE_0__["__metadata"]("design:type", Object)
    ], PublicAccountComponent.prototype, "allowMenu", void 0);
    tslib__WEBPACK_IMPORTED_MODULE_0__["__decorate"]([
        Object(_angular_core__WEBPACK_IMPORTED_MODULE_1__["Input"])("menu"),
        tslib__WEBPACK_IMPORTED_MODULE_0__["__metadata"]("design:type", _angular_core__WEBPACK_IMPORTED_MODULE_1__["ElementRef"]
        /**
         * the observable of the displayed user
         */
        )
    ], PublicAccountComponent.prototype, "menu", void 0);
    PublicAccountComponent = tslib__WEBPACK_IMPORTED_MODULE_0__["__decorate"]([
        Object(_angular_core__WEBPACK_IMPORTED_MODULE_1__["Component"])({
            selector: 'app-public-account',
            template: __webpack_require__(/*! ./public-account.component.html */ "./src/app/components/account/public-account/public-account.component.html"),
            styles: [__webpack_require__(/*! ./public-account.component.scss */ "./src/app/components/account/public-account/public-account.component.scss")]
        }),
        tslib__WEBPACK_IMPORTED_MODULE_0__["__metadata"]("design:paramtypes", [src_app_core_services_account_service__WEBPACK_IMPORTED_MODULE_2__["AccountService"]])
    ], PublicAccountComponent);
    return PublicAccountComponent;
}());



/***/ }),

/***/ "./src/app/components/account/user/user.component.html":
/*!*************************************************************!*\
  !*** ./src/app/components/account/user/user.component.html ***!
  \*************************************************************/
/*! no static exports found */
/***/ (function(module, exports) {

module.exports = "<div>\n  <div *ngIf=\"uid | async as $uid\">\n    <app-public-account [uid]=\"$uid\" [allowMenu]=\"true\" [menu]=\"userMenu\"></app-public-account>\n\n    <mat-menu #userMenu=\"matMenu\">\n      <button mat-menu-item>\n        <div *ngIf=\"friends.uids && friends.uids.indexOf($uid) != -1; else addFriend\">\n          <div (click)=\"friends.removeFriend($uid)\">\n            <mat-icon>person_add_disabled</mat-icon> Remove friend\n          </div>\n        </div>\n      </button>\n    </mat-menu>\n\n    <ng-template #addFriend>\n      <div (click)=\"friends.addFriend($uid)\">\n        <mat-icon>person</mat-icon> Add friend\n      </div>\n    </ng-template>\n  </div>\n</div>"

/***/ }),

/***/ "./src/app/components/account/user/user.component.scss":
/*!*************************************************************!*\
  !*** ./src/app/components/account/user/user.component.scss ***!
  \*************************************************************/
/*! no static exports found */
/***/ (function(module, exports) {

module.exports = "\n/*# sourceMappingURL=data:application/json;base64,eyJ2ZXJzaW9uIjozLCJzb3VyY2VzIjpbXSwibmFtZXMiOltdLCJtYXBwaW5ncyI6IiIsImZpbGUiOiJzcmMvYXBwL2NvbXBvbmVudHMvYWNjb3VudC91c2VyL3VzZXIuY29tcG9uZW50LnNjc3MifQ== */"

/***/ }),

/***/ "./src/app/components/account/user/user.component.ts":
/*!***********************************************************!*\
  !*** ./src/app/components/account/user/user.component.ts ***!
  \***********************************************************/
/*! exports provided: UserComponent */
/***/ (function(module, __webpack_exports__, __webpack_require__) {

"use strict";
__webpack_require__.r(__webpack_exports__);
/* harmony export (binding) */ __webpack_require__.d(__webpack_exports__, "UserComponent", function() { return UserComponent; });
/* harmony import */ var tslib__WEBPACK_IMPORTED_MODULE_0__ = __webpack_require__(/*! tslib */ "./node_modules/tslib/tslib.es6.js");
/* harmony import */ var _angular_core__WEBPACK_IMPORTED_MODULE_1__ = __webpack_require__(/*! @angular/core */ "./node_modules/@angular/core/fesm5/core.js");
/* harmony import */ var src_app_core_services_auth_service__WEBPACK_IMPORTED_MODULE_2__ = __webpack_require__(/*! src/app/core/services/auth.service */ "./src/app/core/services/auth.service.ts");
/* harmony import */ var src_app_core_services_account_service__WEBPACK_IMPORTED_MODULE_3__ = __webpack_require__(/*! src/app/core/services/account.service */ "./src/app/core/services/account.service.ts");
/* harmony import */ var _angular_router__WEBPACK_IMPORTED_MODULE_4__ = __webpack_require__(/*! @angular/router */ "./node_modules/@angular/router/fesm5/router.js");
/* harmony import */ var rxjs_operators__WEBPACK_IMPORTED_MODULE_5__ = __webpack_require__(/*! rxjs/operators */ "./node_modules/rxjs/_esm5/operators/index.js");
/* harmony import */ var src_app_core_services_friend_system_service__WEBPACK_IMPORTED_MODULE_6__ = __webpack_require__(/*! src/app/core/services/friend-system.service */ "./src/app/core/services/friend-system.service.ts");







var UserComponent = /** @class */ (function () {
    function UserComponent(auth, pas, router, route, friends) {
        this.auth = auth;
        this.pas = pas;
        this.router = router;
        this.route = route;
        this.friends = friends;
    }
    /**
     * runs when the component is inited
     */
    UserComponent.prototype.ngOnInit = function () {
        var _this = this;
        //get observable
        this.uid = this.route.paramMap.pipe(Object(rxjs_operators__WEBPACK_IMPORTED_MODULE_5__["map"])(function (params) {
            return params.get("uid");
        }));
        this.user = this.route.paramMap.pipe(Object(rxjs_operators__WEBPACK_IMPORTED_MODULE_5__["switchMap"])(function (params) {
            return _this.pas.getUser(params.get("uid"));
        }));
    };
    UserComponent = tslib__WEBPACK_IMPORTED_MODULE_0__["__decorate"]([
        Object(_angular_core__WEBPACK_IMPORTED_MODULE_1__["Component"])({
            selector: 'app-user',
            template: __webpack_require__(/*! ./user.component.html */ "./src/app/components/account/user/user.component.html"),
            styles: [__webpack_require__(/*! ./user.component.scss */ "./src/app/components/account/user/user.component.scss")]
        }),
        tslib__WEBPACK_IMPORTED_MODULE_0__["__metadata"]("design:paramtypes", [src_app_core_services_auth_service__WEBPACK_IMPORTED_MODULE_2__["AuthService"],
            src_app_core_services_account_service__WEBPACK_IMPORTED_MODULE_3__["AccountService"],
            _angular_router__WEBPACK_IMPORTED_MODULE_4__["Router"],
            _angular_router__WEBPACK_IMPORTED_MODULE_4__["ActivatedRoute"],
            src_app_core_services_friend_system_service__WEBPACK_IMPORTED_MODULE_6__["FriendSystemService"]])
    ], UserComponent);
    return UserComponent;
}());



/***/ }),

/***/ "./src/app/components/account/users/users.component.html":
/*!***************************************************************!*\
  !*** ./src/app/components/account/users/users.component.html ***!
  \***************************************************************/
/*! no static exports found */
/***/ (function(module, exports) {

module.exports = "<p>\n  users works!\n</p>\n"

/***/ }),

/***/ "./src/app/components/account/users/users.component.scss":
/*!***************************************************************!*\
  !*** ./src/app/components/account/users/users.component.scss ***!
  \***************************************************************/
/*! no static exports found */
/***/ (function(module, exports) {

module.exports = "\n/*# sourceMappingURL=data:application/json;base64,eyJ2ZXJzaW9uIjozLCJzb3VyY2VzIjpbXSwibmFtZXMiOltdLCJtYXBwaW5ncyI6IiIsImZpbGUiOiJzcmMvYXBwL2NvbXBvbmVudHMvYWNjb3VudC91c2Vycy91c2Vycy5jb21wb25lbnQuc2NzcyJ9 */"

/***/ }),

/***/ "./src/app/components/account/users/users.component.ts":
/*!*************************************************************!*\
  !*** ./src/app/components/account/users/users.component.ts ***!
  \*************************************************************/
/*! exports provided: UsersComponent */
/***/ (function(module, __webpack_exports__, __webpack_require__) {

"use strict";
__webpack_require__.r(__webpack_exports__);
/* harmony export (binding) */ __webpack_require__.d(__webpack_exports__, "UsersComponent", function() { return UsersComponent; });
/* harmony import */ var tslib__WEBPACK_IMPORTED_MODULE_0__ = __webpack_require__(/*! tslib */ "./node_modules/tslib/tslib.es6.js");
/* harmony import */ var _angular_core__WEBPACK_IMPORTED_MODULE_1__ = __webpack_require__(/*! @angular/core */ "./node_modules/@angular/core/fesm5/core.js");


var UsersComponent = /** @class */ (function () {
    function UsersComponent() {
    }
    UsersComponent.prototype.ngOnInit = function () {
    };
    UsersComponent = tslib__WEBPACK_IMPORTED_MODULE_0__["__decorate"]([
        Object(_angular_core__WEBPACK_IMPORTED_MODULE_1__["Component"])({
            selector: 'app-users',
            template: __webpack_require__(/*! ./users.component.html */ "./src/app/components/account/users/users.component.html"),
            styles: [__webpack_require__(/*! ./users.component.scss */ "./src/app/components/account/users/users.component.scss")]
        }),
        tslib__WEBPACK_IMPORTED_MODULE_0__["__metadata"]("design:paramtypes", [])
    ], UsersComponent);
    return UsersComponent;
}());



/***/ }),

/***/ "./src/app/components/auth/login/login.component.html":
/*!************************************************************!*\
  !*** ./src/app/components/auth/login/login.component.html ***!
  \************************************************************/
/*! no static exports found */
/***/ (function(module, exports) {

module.exports = "<div>\n  <div class=\"full\" color=\"primary\">\n    <div *ngIf=\"auth.user | async; then authenticated else guest\">\n\n    </div>\n  </div>\n</div>\n\n\n<ng-template #authenticated>\n //waiting for login\n</ng-template>\n\n\n<ng-template #guest>\n  <mat-card class=\"login-cont\">\n    <mat-card-header>\n      <mat-card-title class=\"auth-title-container\">\n        <div class=\"auth-title\">\n          Login to get started\n        </div>\n      </mat-card-title>\n    </mat-card-header>\n    <mat-card-content>\n      <mat-list>\n        <div class=\"auth-list-container\">\n          <mat-list-item>\n            <button mat-raised-button color=\"primary\" (click)=\"auth.login('google')\" mat-button>\n              Google auth\n            </button>\n          </mat-list-item>\n          <mat-list-item>\n            <button mat-raised-button color=\"basic\" (click)=\"auth.login('github')\" mat-button>\n              Github auth\n            </button>\n          </mat-list-item>\n        </div>\n      </mat-list>\n    </mat-card-content>\n  </mat-card>\n</ng-template>"

/***/ }),

/***/ "./src/app/components/auth/login/login.component.scss":
/*!************************************************************!*\
  !*** ./src/app/components/auth/login/login.component.scss ***!
  \************************************************************/
/*! no static exports found */
/***/ (function(module, exports) {

module.exports = ".avatar {\n  background-size: cover; }\n\n.full {\n  height: 100%;\n  display: flex;\n  flex-direction: column;\n  justify-content: center;\n  align-items: center;\n  text-align: center;\n  overflow: hidden; }\n\n.login-cont {\n  width: 40%; }\n\n.mat-card-header, .auth-list-container, .mat-list-item {\n  display: flex;\n  justify-content: center;\n  align-items: center;\n  text-align: center;\n  flex-direction: column; }\n\n@media only screen and (max-width: 700px) {\n  .login-cont {\n    width: 100%; } }\n\n@media only screen and (max-width: 400px) {\n  .login-cont {\n    height: 100%;\n    display: flex;\n    justify-content: center;\n    align-items: center;\n    text-align: center;\n    flex-direction: column; } }\n\n/*# sourceMappingURL=data:application/json;base64,eyJ2ZXJzaW9uIjozLCJzb3VyY2VzIjpbInNyYy9hcHAvY29tcG9uZW50cy9hdXRoL2xvZ2luL2M6XFxVc2Vyc1xcbGFyeW1cXHJlcG9zXFxhbmd1bGFyLWNsaWVudC9zcmNcXGFwcFxcY29tcG9uZW50c1xcYXV0aFxcbG9naW5cXGxvZ2luLmNvbXBvbmVudC5zY3NzIl0sIm5hbWVzIjpbXSwibWFwcGluZ3MiOiJBQUFBO0VBQ0ksc0JBQXNCLEVBQUE7O0FBRTFCO0VBQ0ksWUFBVztFQUNYLGFBQVk7RUFDWixzQkFBc0I7RUFDdEIsdUJBQXVCO0VBQ3ZCLG1CQUFtQjtFQUNuQixrQkFBa0I7RUFDbEIsZ0JBQWdCLEVBQUE7O0FBR3BCO0VBQ0ksVUFBUyxFQUFBOztBQUViO0VBQ0ksYUFBYTtFQUNiLHVCQUF1QjtFQUN2QixtQkFBbUI7RUFDbkIsa0JBQWtCO0VBQ2xCLHNCQUFzQixFQUFBOztBQUcxQjtFQUNJO0lBQ0ksV0FBVSxFQUFBLEVBQ2I7O0FBRUw7RUFDSTtJQUNJLFlBQVc7SUFDWCxhQUFhO0lBQ2IsdUJBQXVCO0lBQ3ZCLG1CQUFtQjtJQUNuQixrQkFBa0I7SUFDbEIsc0JBQXNCLEVBQUEsRUFDekIiLCJmaWxlIjoic3JjL2FwcC9jb21wb25lbnRzL2F1dGgvbG9naW4vbG9naW4uY29tcG9uZW50LnNjc3MiLCJzb3VyY2VzQ29udGVudCI6WyIuYXZhdGFye1xyXG4gICAgYmFja2dyb3VuZC1zaXplOiBjb3ZlcjtcclxufVxyXG4uZnVsbHtcclxuICAgIGhlaWdodDoxMDAlO1xyXG4gICAgZGlzcGxheTpmbGV4O1xyXG4gICAgZmxleC1kaXJlY3Rpb246IGNvbHVtbjtcclxuICAgIGp1c3RpZnktY29udGVudDogY2VudGVyO1xyXG4gICAgYWxpZ24taXRlbXM6IGNlbnRlcjtcclxuICAgIHRleHQtYWxpZ246IGNlbnRlcjtcclxuICAgIG92ZXJmbG93OiBoaWRkZW47XHJcbn1cclxuXHJcbi5sb2dpbi1jb250e1xyXG4gICAgd2lkdGg6NDAlO1xyXG59XHJcbi5tYXQtY2FyZC1oZWFkZXIsLmF1dGgtbGlzdC1jb250YWluZXIsLm1hdC1saXN0LWl0ZW17XHJcbiAgICBkaXNwbGF5OiBmbGV4O1xyXG4gICAganVzdGlmeS1jb250ZW50OiBjZW50ZXI7XHJcbiAgICBhbGlnbi1pdGVtczogY2VudGVyO1xyXG4gICAgdGV4dC1hbGlnbjogY2VudGVyO1xyXG4gICAgZmxleC1kaXJlY3Rpb246IGNvbHVtbjtcclxufVxyXG5cclxuQG1lZGlhIG9ubHkgc2NyZWVuIGFuZCAobWF4LXdpZHRoOiA3MDBweCl7XHJcbiAgICAubG9naW4tY29udHtcclxuICAgICAgICB3aWR0aDoxMDAlO1xyXG4gICAgfVxyXG59XHJcbkBtZWRpYSBvbmx5IHNjcmVlbiBhbmQgKG1heC13aWR0aDogNDAwcHgpe1xyXG4gICAgLmxvZ2luLWNvbnR7XHJcbiAgICAgICAgaGVpZ2h0OjEwMCU7XHJcbiAgICAgICAgZGlzcGxheTogZmxleDtcclxuICAgICAgICBqdXN0aWZ5LWNvbnRlbnQ6IGNlbnRlcjtcclxuICAgICAgICBhbGlnbi1pdGVtczogY2VudGVyO1xyXG4gICAgICAgIHRleHQtYWxpZ246IGNlbnRlcjtcclxuICAgICAgICBmbGV4LWRpcmVjdGlvbjogY29sdW1uO1xyXG4gICAgfVxyXG59Il19 */"

/***/ }),

/***/ "./src/app/components/auth/login/login.component.ts":
/*!**********************************************************!*\
  !*** ./src/app/components/auth/login/login.component.ts ***!
  \**********************************************************/
/*! exports provided: LoginComponent */
/***/ (function(module, __webpack_exports__, __webpack_require__) {

"use strict";
__webpack_require__.r(__webpack_exports__);
/* harmony export (binding) */ __webpack_require__.d(__webpack_exports__, "LoginComponent", function() { return LoginComponent; });
/* harmony import */ var tslib__WEBPACK_IMPORTED_MODULE_0__ = __webpack_require__(/*! tslib */ "./node_modules/tslib/tslib.es6.js");
/* harmony import */ var _angular_core__WEBPACK_IMPORTED_MODULE_1__ = __webpack_require__(/*! @angular/core */ "./node_modules/@angular/core/fesm5/core.js");
/* harmony import */ var src_app_core_services_auth_service__WEBPACK_IMPORTED_MODULE_2__ = __webpack_require__(/*! src/app/core/services/auth.service */ "./src/app/core/services/auth.service.ts");
/* harmony import */ var rxjs_operators__WEBPACK_IMPORTED_MODULE_3__ = __webpack_require__(/*! rxjs/operators */ "./node_modules/rxjs/_esm5/operators/index.js");
/* harmony import */ var _angular_router__WEBPACK_IMPORTED_MODULE_4__ = __webpack_require__(/*! @angular/router */ "./node_modules/@angular/router/fesm5/router.js");





var LoginComponent = /** @class */ (function () {
    function LoginComponent(auth, router) {
        this.auth = auth;
        this.router = router;
    }
    LoginComponent.prototype.ngOnInit = function () {
        var _this = this;
        this.auth.user.pipe(Object(rxjs_operators__WEBPACK_IMPORTED_MODULE_3__["filter"])(function (value) { return !!value; })).subscribe(function (value) { return _this.router.navigate(["/account"]); });
    };
    LoginComponent = tslib__WEBPACK_IMPORTED_MODULE_0__["__decorate"]([
        Object(_angular_core__WEBPACK_IMPORTED_MODULE_1__["Component"])({
            selector: 'app-login',
            template: __webpack_require__(/*! ./login.component.html */ "./src/app/components/auth/login/login.component.html"),
            styles: [__webpack_require__(/*! ./login.component.scss */ "./src/app/components/auth/login/login.component.scss")]
        }),
        tslib__WEBPACK_IMPORTED_MODULE_0__["__metadata"]("design:paramtypes", [src_app_core_services_auth_service__WEBPACK_IMPORTED_MODULE_2__["AuthService"], _angular_router__WEBPACK_IMPORTED_MODULE_4__["Router"]])
    ], LoginComponent);
    return LoginComponent;
}());



/***/ }),

/***/ "./src/app/components/logging/log-provider.service.ts":
/*!************************************************************!*\
  !*** ./src/app/components/logging/log-provider.service.ts ***!
  \************************************************************/
/*! exports provided: LogProviderService */
/***/ (function(module, __webpack_exports__, __webpack_require__) {

"use strict";
__webpack_require__.r(__webpack_exports__);
/* harmony export (binding) */ __webpack_require__.d(__webpack_exports__, "LogProviderService", function() { return LogProviderService; });
/* harmony import */ var tslib__WEBPACK_IMPORTED_MODULE_0__ = __webpack_require__(/*! tslib */ "./node_modules/tslib/tslib.es6.js");
/* harmony import */ var _angular_core__WEBPACK_IMPORTED_MODULE_1__ = __webpack_require__(/*! @angular/core */ "./node_modules/@angular/core/fesm5/core.js");
/* harmony import */ var _angular_common_http__WEBPACK_IMPORTED_MODULE_2__ = __webpack_require__(/*! @angular/common/http */ "./node_modules/@angular/common/fesm5/http.js");



var LogProviderService = /** @class */ (function () {
    function LogProviderService(http) {
        this.http = http;
        this.baseUrl = "http://localhost:8000";
        this.path = "logs";
    }
    LogProviderService.prototype.getLogs = function () {
        return this.http.get(this.baseUrl + "/" + this.path);
    };
    LogProviderService = tslib__WEBPACK_IMPORTED_MODULE_0__["__decorate"]([
        Object(_angular_core__WEBPACK_IMPORTED_MODULE_1__["Injectable"])({
            providedIn: 'root'
        }),
        tslib__WEBPACK_IMPORTED_MODULE_0__["__metadata"]("design:paramtypes", [_angular_common_http__WEBPACK_IMPORTED_MODULE_2__["HttpClient"]])
    ], LogProviderService);
    return LogProviderService;
}());



/***/ }),

/***/ "./src/app/components/logging/log/log.component.html":
/*!***********************************************************!*\
  !*** ./src/app/components/logging/log/log.component.html ***!
  \***********************************************************/
/*! no static exports found */
/***/ (function(module, exports) {

module.exports = "<mat-card class=\"logo-mat-card\">\n  <mat-card-header>\n    <mat-card-title>{{data.title}}</mat-card-title>\n  </mat-card-header>\n\n  <mat-card-content>\n    {{data.body}}\n  </mat-card-content>\n</mat-card>"

/***/ }),

/***/ "./src/app/components/logging/log/log.component.scss":
/*!***********************************************************!*\
  !*** ./src/app/components/logging/log/log.component.scss ***!
  \***********************************************************/
/*! no static exports found */
/***/ (function(module, exports) {

module.exports = "\n/*# sourceMappingURL=data:application/json;base64,eyJ2ZXJzaW9uIjozLCJzb3VyY2VzIjpbXSwibmFtZXMiOltdLCJtYXBwaW5ncyI6IiIsImZpbGUiOiJzcmMvYXBwL2NvbXBvbmVudHMvbG9nZ2luZy9sb2cvbG9nLmNvbXBvbmVudC5zY3NzIn0= */"

/***/ }),

/***/ "./src/app/components/logging/log/log.component.ts":
/*!*********************************************************!*\
  !*** ./src/app/components/logging/log/log.component.ts ***!
  \*********************************************************/
/*! exports provided: LogComponent */
/***/ (function(module, __webpack_exports__, __webpack_require__) {

"use strict";
__webpack_require__.r(__webpack_exports__);
/* harmony export (binding) */ __webpack_require__.d(__webpack_exports__, "LogComponent", function() { return LogComponent; });
/* harmony import */ var tslib__WEBPACK_IMPORTED_MODULE_0__ = __webpack_require__(/*! tslib */ "./node_modules/tslib/tslib.es6.js");
/* harmony import */ var _angular_core__WEBPACK_IMPORTED_MODULE_1__ = __webpack_require__(/*! @angular/core */ "./node_modules/@angular/core/fesm5/core.js");


var LogComponent = /** @class */ (function () {
    function LogComponent() {
    }
    LogComponent.prototype.ngOnInit = function () {
        console.log(this.data);
    };
    tslib__WEBPACK_IMPORTED_MODULE_0__["__decorate"]([
        Object(_angular_core__WEBPACK_IMPORTED_MODULE_1__["Input"])(),
        tslib__WEBPACK_IMPORTED_MODULE_0__["__metadata"]("design:type", Object)
    ], LogComponent.prototype, "data", void 0);
    LogComponent = tslib__WEBPACK_IMPORTED_MODULE_0__["__decorate"]([
        Object(_angular_core__WEBPACK_IMPORTED_MODULE_1__["Component"])({
            selector: 'app-log',
            template: __webpack_require__(/*! ./log.component.html */ "./src/app/components/logging/log/log.component.html"),
            styles: [__webpack_require__(/*! ./log.component.scss */ "./src/app/components/logging/log/log.component.scss")]
        }),
        tslib__WEBPACK_IMPORTED_MODULE_0__["__metadata"]("design:paramtypes", [])
    ], LogComponent);
    return LogComponent;
}());



/***/ }),

/***/ "./src/app/components/logging/logs/logs.component.html":
/*!*************************************************************!*\
  !*** ./src/app/components/logging/logs/logs.component.html ***!
  \*************************************************************/
/*! no static exports found */
/***/ (function(module, exports) {

module.exports = "<div class=\"logs-container\">\n  <div>\n    <div class=\"log-innter-container\" *ngFor=\"let log of logs | async\">\n      <app-log [data]=\"log\"></app-log>\n    </div>\n  </div>\n  <div class=\"more-logs\">\n    More may or may not be logged later!\n  </div>\n</div>"

/***/ }),

/***/ "./src/app/components/logging/logs/logs.component.scss":
/*!*************************************************************!*\
  !*** ./src/app/components/logging/logs/logs.component.scss ***!
  \*************************************************************/
/*! no static exports found */
/***/ (function(module, exports) {

module.exports = ".more-logs {\n  width: 100%;\n  text-align: center;\n  color: rgba(255, 255, 255, 0.5);\n  font-size: 75%; }\n\n/*# sourceMappingURL=data:application/json;base64,eyJ2ZXJzaW9uIjozLCJzb3VyY2VzIjpbInNyYy9hcHAvY29tcG9uZW50cy9sb2dnaW5nL2xvZ3MvYzpcXFVzZXJzXFxsYXJ5bVxccmVwb3NcXGFuZ3VsYXItY2xpZW50L3NyY1xcYXBwXFxjb21wb25lbnRzXFxsb2dnaW5nXFxsb2dzXFxsb2dzLmNvbXBvbmVudC5zY3NzIl0sIm5hbWVzIjpbXSwibWFwcGluZ3MiOiJBQUFBO0VBQ0ksV0FBVTtFQUNWLGtCQUFrQjtFQUNsQiwrQkFBMEI7RUFDMUIsY0FBYyxFQUFBIiwiZmlsZSI6InNyYy9hcHAvY29tcG9uZW50cy9sb2dnaW5nL2xvZ3MvbG9ncy5jb21wb25lbnQuc2NzcyIsInNvdXJjZXNDb250ZW50IjpbIi5tb3JlLWxvZ3N7XHJcbiAgICB3aWR0aDoxMDAlO1xyXG4gICAgdGV4dC1hbGlnbjogY2VudGVyO1xyXG4gICAgY29sb3I6cmdiYSgyNTYsMjU2LDI1NiwuNSk7XHJcbiAgICBmb250LXNpemU6IDc1JTtcclxufSJdfQ== */"

/***/ }),

/***/ "./src/app/components/logging/logs/logs.component.ts":
/*!***********************************************************!*\
  !*** ./src/app/components/logging/logs/logs.component.ts ***!
  \***********************************************************/
/*! exports provided: LogsComponent */
/***/ (function(module, __webpack_exports__, __webpack_require__) {

"use strict";
__webpack_require__.r(__webpack_exports__);
/* harmony export (binding) */ __webpack_require__.d(__webpack_exports__, "LogsComponent", function() { return LogsComponent; });
/* harmony import */ var tslib__WEBPACK_IMPORTED_MODULE_0__ = __webpack_require__(/*! tslib */ "./node_modules/tslib/tslib.es6.js");
/* harmony import */ var _angular_core__WEBPACK_IMPORTED_MODULE_1__ = __webpack_require__(/*! @angular/core */ "./node_modules/@angular/core/fesm5/core.js");
/* harmony import */ var _log_provider_service__WEBPACK_IMPORTED_MODULE_2__ = __webpack_require__(/*! ../log-provider.service */ "./src/app/components/logging/log-provider.service.ts");



var LogsComponent = /** @class */ (function () {
    function LogsComponent(source) {
        this.source = source;
    }
    LogsComponent.prototype.ngOnInit = function () {
        this.logs = this.source.getLogs();
    };
    LogsComponent = tslib__WEBPACK_IMPORTED_MODULE_0__["__decorate"]([
        Object(_angular_core__WEBPACK_IMPORTED_MODULE_1__["Component"])({
            selector: 'app-logs',
            template: __webpack_require__(/*! ./logs.component.html */ "./src/app/components/logging/logs/logs.component.html"),
            styles: [__webpack_require__(/*! ./logs.component.scss */ "./src/app/components/logging/logs/logs.component.scss")]
        }),
        tslib__WEBPACK_IMPORTED_MODULE_0__["__metadata"]("design:paramtypes", [_log_provider_service__WEBPACK_IMPORTED_MODULE_2__["LogProviderService"]])
    ], LogsComponent);
    return LogsComponent;
}());



/***/ }),

/***/ "./src/app/components/menu/menu.component.html":
/*!*****************************************************!*\
  !*** ./src/app/components/menu/menu.component.html ***!
  \*****************************************************/
/*! no static exports found */
/***/ (function(module, exports) {

module.exports = "<div class=\"full\" color=\"primary\">\n  <mat-list>\n    <mat-list-item>\n      <div class=\"avatar-wrapper\">\n        <div *ngIf=\"auth.user | async; then authenticated else guest\"></div>\n      </div>\n    </mat-list-item>\n    <div *ngFor=\"let menuRoute of menuRoutes\">\n      <a [routerLink]=\"menuRoute.path\" matTooltipPosition=\"after\" [matTooltip]=\"menuRoute.description\">\n        <mat-list-item>\n          <h4 mat-line class=\"menu-item-name\">{{menuRoute.name}}</h4>\n          <mat-icon mat-list-icon>{{menuRoute.icon}}</mat-icon>\n        </mat-list-item>\n      </a>\n    </div>\n  </mat-list>\n</div>\n\n\n<mat-menu #accMenu=\"matMenu\">\n  <button mat-menu-item (click)=\"auth.logout()\">\n    <mat-icon>arrow_right_alt</mat-icon>\n    <span>Logout</span>\n  </button>\n  <button mat-menu-item>\n    <mat-icon>account_circle</mat-icon>\n    <span>Profile</span>\n  </button>\n</mat-menu>\n\n\n<ng-template #authenticated>\n  <div *ngIf=\"auth.user | async as user\">\n    <div mat-card-avatar class=avatar [style.background-image]=\"'url(' + user.photoUrl + ')'\"\n      [matMenuTriggerFor]=\"accMenu\">\n    </div>\n  </div>\n</ng-template>\n\n\n<ng-template #guest>\n  <button mat-mini-fab class=\"menu-login\">\n    <mat-icon>person</mat-icon>\n  </button>\n</ng-template>"

/***/ }),

/***/ "./src/app/components/menu/menu.component.scss":
/*!*****************************************************!*\
  !*** ./src/app/components/menu/menu.component.scss ***!
  \*****************************************************/
/*! no static exports found */
/***/ (function(module, exports) {

module.exports = ".avata-wrapper {\n  display: flex;\n  justify-content: center;\n  flex-direction: column;\n  align-items: center; }\n\na {\n  text-decoration: none;\n  color: white;\n  font-style: normal; }\n\n@media only screen and (max-width: 720px) {\n  .menu-item-name {\n    opacity: 0;\n    width: 0px;\n    height: 0px; } }\n\n/*# sourceMappingURL=data:application/json;base64,eyJ2ZXJzaW9uIjozLCJzb3VyY2VzIjpbInNyYy9hcHAvY29tcG9uZW50cy9tZW51L2M6XFxVc2Vyc1xcbGFyeW1cXHJlcG9zXFxhbmd1bGFyLWNsaWVudC9zcmNcXGFwcFxcY29tcG9uZW50c1xcbWVudVxcbWVudS5jb21wb25lbnQuc2NzcyJdLCJuYW1lcyI6W10sIm1hcHBpbmdzIjoiQUFBQTtFQUNJLGFBQWE7RUFDYix1QkFBdUI7RUFDdkIsc0JBQXNCO0VBQ3RCLG1CQUFtQixFQUFBOztBQUV2QjtFQUNJLHFCQUFxQjtFQUNyQixZQUFXO0VBQ1gsa0JBQWtCLEVBQUE7O0FBRXRCO0VBQ0k7SUFDSSxVQUFVO0lBQ1YsVUFBVTtJQUNWLFdBQVcsRUFBQSxFQUNkIiwiZmlsZSI6InNyYy9hcHAvY29tcG9uZW50cy9tZW51L21lbnUuY29tcG9uZW50LnNjc3MiLCJzb3VyY2VzQ29udGVudCI6WyIuYXZhdGEtd3JhcHBlcntcclxuICAgIGRpc3BsYXk6IGZsZXg7XHJcbiAgICBqdXN0aWZ5LWNvbnRlbnQ6IGNlbnRlcjtcclxuICAgIGZsZXgtZGlyZWN0aW9uOiBjb2x1bW47XHJcbiAgICBhbGlnbi1pdGVtczogY2VudGVyO1xyXG59XHJcbmF7XHJcbiAgICB0ZXh0LWRlY29yYXRpb246IG5vbmU7XHJcbiAgICBjb2xvcjp3aGl0ZTtcclxuICAgIGZvbnQtc3R5bGU6IG5vcm1hbDtcclxufVxyXG5AbWVkaWEgb25seSBzY3JlZW4gYW5kIChtYXgtd2lkdGg6NzIwcHgpIHtcclxuICAgIC5tZW51LWl0ZW0tbmFtZXtcclxuICAgICAgICBvcGFjaXR5OiAwO1xyXG4gICAgICAgIHdpZHRoOiAwcHg7XHJcbiAgICAgICAgaGVpZ2h0OiAwcHg7XHJcbiAgICB9ICAgIFxyXG59XHJcbiJdfQ== */"

/***/ }),

/***/ "./src/app/components/menu/menu.component.ts":
/*!***************************************************!*\
  !*** ./src/app/components/menu/menu.component.ts ***!
  \***************************************************/
/*! exports provided: MenuComponent */
/***/ (function(module, __webpack_exports__, __webpack_require__) {

"use strict";
__webpack_require__.r(__webpack_exports__);
/* harmony export (binding) */ __webpack_require__.d(__webpack_exports__, "MenuComponent", function() { return MenuComponent; });
/* harmony import */ var tslib__WEBPACK_IMPORTED_MODULE_0__ = __webpack_require__(/*! tslib */ "./node_modules/tslib/tslib.es6.js");
/* harmony import */ var _angular_core__WEBPACK_IMPORTED_MODULE_1__ = __webpack_require__(/*! @angular/core */ "./node_modules/@angular/core/fesm5/core.js");
/* harmony import */ var src_app_core_services_auth_service__WEBPACK_IMPORTED_MODULE_2__ = __webpack_require__(/*! src/app/core/services/auth.service */ "./src/app/core/services/auth.service.ts");



var MenuComponent = /** @class */ (function () {
    function MenuComponent(auth) {
        this.auth = auth;
        this.menuRoutes = [{
                path: "account",
                icon: "person",
                name: "account",
                description: "all about you"
            }, {
                path: "logs",
                icon: "library_books",
                name: "logs",
                description: "server logs"
            }];
    }
    MenuComponent.prototype.ngOnInit = function () {
    };
    MenuComponent = tslib__WEBPACK_IMPORTED_MODULE_0__["__decorate"]([
        Object(_angular_core__WEBPACK_IMPORTED_MODULE_1__["Component"])({
            selector: 'app-menu',
            template: __webpack_require__(/*! ./menu.component.html */ "./src/app/components/menu/menu.component.html"),
            styles: [__webpack_require__(/*! ./menu.component.scss */ "./src/app/components/menu/menu.component.scss")]
        }),
        tslib__WEBPACK_IMPORTED_MODULE_0__["__metadata"]("design:paramtypes", [src_app_core_services_auth_service__WEBPACK_IMPORTED_MODULE_2__["AuthService"]])
    ], MenuComponent);
    return MenuComponent;
}());



/***/ }),

/***/ "./src/app/core/core.module.ts":
/*!*************************************!*\
  !*** ./src/app/core/core.module.ts ***!
  \*************************************/
/*! exports provided: CoreModule */
/***/ (function(module, __webpack_exports__, __webpack_require__) {

"use strict";
__webpack_require__.r(__webpack_exports__);
/* harmony export (binding) */ __webpack_require__.d(__webpack_exports__, "CoreModule", function() { return CoreModule; });
/* harmony import */ var tslib__WEBPACK_IMPORTED_MODULE_0__ = __webpack_require__(/*! tslib */ "./node_modules/tslib/tslib.es6.js");
/* harmony import */ var _angular_core__WEBPACK_IMPORTED_MODULE_1__ = __webpack_require__(/*! @angular/core */ "./node_modules/@angular/core/fesm5/core.js");
/* harmony import */ var _angular_common__WEBPACK_IMPORTED_MODULE_2__ = __webpack_require__(/*! @angular/common */ "./node_modules/@angular/common/fesm5/common.js");
/* harmony import */ var _material_material_module__WEBPACK_IMPORTED_MODULE_3__ = __webpack_require__(/*! ./material/material.module */ "./src/app/core/material/material.module.ts");
/* harmony import */ var _angular_fire_auth__WEBPACK_IMPORTED_MODULE_4__ = __webpack_require__(/*! @angular/fire/auth */ "./node_modules/@angular/fire/auth/index.js");
/* harmony import */ var _angular_fire_firestore__WEBPACK_IMPORTED_MODULE_5__ = __webpack_require__(/*! @angular/fire/firestore */ "./node_modules/@angular/fire/firestore/index.js");
/* harmony import */ var _services_auth_service__WEBPACK_IMPORTED_MODULE_6__ = __webpack_require__(/*! ./services/auth.service */ "./src/app/core/services/auth.service.ts");
/* harmony import */ var _components_account_account_account_component__WEBPACK_IMPORTED_MODULE_7__ = __webpack_require__(/*! ../components/account/account/account.component */ "./src/app/components/account/account/account.component.ts");
/* harmony import */ var _components_account_public_account_public_account_component__WEBPACK_IMPORTED_MODULE_8__ = __webpack_require__(/*! ../components/account/public-account/public-account.component */ "./src/app/components/account/public-account/public-account.component.ts");
/* harmony import */ var _components_account_users_users_component__WEBPACK_IMPORTED_MODULE_9__ = __webpack_require__(/*! ../components/account/users/users.component */ "./src/app/components/account/users/users.component.ts");
/* harmony import */ var _components_account_user_user_component__WEBPACK_IMPORTED_MODULE_10__ = __webpack_require__(/*! ../components/account/user/user.component */ "./src/app/components/account/user/user.component.ts");
/* harmony import */ var _app_routing_module__WEBPACK_IMPORTED_MODULE_11__ = __webpack_require__(/*! ../app-routing.module */ "./src/app/app-routing.module.ts");












var CoreModule = /** @class */ (function () {
    function CoreModule() {
    }
    CoreModule = tslib__WEBPACK_IMPORTED_MODULE_0__["__decorate"]([
        Object(_angular_core__WEBPACK_IMPORTED_MODULE_1__["NgModule"])({
            declarations: [_components_account_account_account_component__WEBPACK_IMPORTED_MODULE_7__["AccountComponent"], _components_account_public_account_public_account_component__WEBPACK_IMPORTED_MODULE_8__["PublicAccountComponent"], _components_account_users_users_component__WEBPACK_IMPORTED_MODULE_9__["UsersComponent"], _components_account_user_user_component__WEBPACK_IMPORTED_MODULE_10__["UserComponent"]],
            imports: [
                _angular_common__WEBPACK_IMPORTED_MODULE_2__["CommonModule"],
                _material_material_module__WEBPACK_IMPORTED_MODULE_3__["MaterialModule"],
                _angular_fire_auth__WEBPACK_IMPORTED_MODULE_4__["AngularFireAuthModule"],
                _angular_fire_firestore__WEBPACK_IMPORTED_MODULE_5__["AngularFirestoreModule"],
                _app_routing_module__WEBPACK_IMPORTED_MODULE_11__["AppRoutingModule"]
            ],
            exports: [
                _material_material_module__WEBPACK_IMPORTED_MODULE_3__["MaterialModule"]
            ],
            providers: [
                _services_auth_service__WEBPACK_IMPORTED_MODULE_6__["AuthService"]
            ]
        })
    ], CoreModule);
    return CoreModule;
}());



/***/ }),

/***/ "./src/app/core/guards/auth-guard.guard.ts":
/*!*************************************************!*\
  !*** ./src/app/core/guards/auth-guard.guard.ts ***!
  \*************************************************/
/*! exports provided: AuthGuard */
/***/ (function(module, __webpack_exports__, __webpack_require__) {

"use strict";
__webpack_require__.r(__webpack_exports__);
/* harmony export (binding) */ __webpack_require__.d(__webpack_exports__, "AuthGuard", function() { return AuthGuard; });
/* harmony import */ var tslib__WEBPACK_IMPORTED_MODULE_0__ = __webpack_require__(/*! tslib */ "./node_modules/tslib/tslib.es6.js");
/* harmony import */ var _angular_core__WEBPACK_IMPORTED_MODULE_1__ = __webpack_require__(/*! @angular/core */ "./node_modules/@angular/core/fesm5/core.js");
/* harmony import */ var _angular_router__WEBPACK_IMPORTED_MODULE_2__ = __webpack_require__(/*! @angular/router */ "./node_modules/@angular/router/fesm5/router.js");
/* harmony import */ var _services_auth_service__WEBPACK_IMPORTED_MODULE_3__ = __webpack_require__(/*! ../services/auth.service */ "./src/app/core/services/auth.service.ts");
/* harmony import */ var rxjs_operators__WEBPACK_IMPORTED_MODULE_4__ = __webpack_require__(/*! rxjs/operators */ "./node_modules/rxjs/_esm5/operators/index.js");





var AuthGuard = /** @class */ (function () {
    function AuthGuard(auth, router) {
        this.auth = auth;
        this.router = router;
    }
    AuthGuard.prototype.canActivate = function (next, state) {
        var _this = this;
        return this.auth.user.pipe(Object(rxjs_operators__WEBPACK_IMPORTED_MODULE_4__["take"])(1), Object(rxjs_operators__WEBPACK_IMPORTED_MODULE_4__["map"])(function (user) { return !!user; }), Object(rxjs_operators__WEBPACK_IMPORTED_MODULE_4__["tap"])(function (loggedIn) {
            if (!loggedIn) {
                console.log("acces denied");
                _this.router.navigate(["/login"]);
            }
        }));
    };
    AuthGuard = tslib__WEBPACK_IMPORTED_MODULE_0__["__decorate"]([
        Object(_angular_core__WEBPACK_IMPORTED_MODULE_1__["Injectable"])({
            providedIn: 'root'
        }),
        tslib__WEBPACK_IMPORTED_MODULE_0__["__metadata"]("design:paramtypes", [_services_auth_service__WEBPACK_IMPORTED_MODULE_3__["AuthService"], _angular_router__WEBPACK_IMPORTED_MODULE_2__["Router"]])
    ], AuthGuard);
    return AuthGuard;
}());



/***/ }),

/***/ "./src/app/core/material/material.module.ts":
/*!**************************************************!*\
  !*** ./src/app/core/material/material.module.ts ***!
  \**************************************************/
/*! exports provided: MaterialModule */
/***/ (function(module, __webpack_exports__, __webpack_require__) {

"use strict";
__webpack_require__.r(__webpack_exports__);
/* harmony export (binding) */ __webpack_require__.d(__webpack_exports__, "MaterialModule", function() { return MaterialModule; });
/* harmony import */ var tslib__WEBPACK_IMPORTED_MODULE_0__ = __webpack_require__(/*! tslib */ "./node_modules/tslib/tslib.es6.js");
/* harmony import */ var _angular_core__WEBPACK_IMPORTED_MODULE_1__ = __webpack_require__(/*! @angular/core */ "./node_modules/@angular/core/fesm5/core.js");
/* harmony import */ var _angular_common__WEBPACK_IMPORTED_MODULE_2__ = __webpack_require__(/*! @angular/common */ "./node_modules/@angular/common/fesm5/common.js");
/* harmony import */ var _angular_material_card__WEBPACK_IMPORTED_MODULE_3__ = __webpack_require__(/*! @angular/material/card */ "./node_modules/@angular/material/esm5/card.es5.js");
/* harmony import */ var _angular_material_button__WEBPACK_IMPORTED_MODULE_4__ = __webpack_require__(/*! @angular/material/button */ "./node_modules/@angular/material/esm5/button.es5.js");
/* harmony import */ var _angular_material_input__WEBPACK_IMPORTED_MODULE_5__ = __webpack_require__(/*! @angular/material/input */ "./node_modules/@angular/material/esm5/input.es5.js");
/* harmony import */ var _angular_material_form_field__WEBPACK_IMPORTED_MODULE_6__ = __webpack_require__(/*! @angular/material/form-field */ "./node_modules/@angular/material/esm5/form-field.es5.js");
/* harmony import */ var _angular_material_icon__WEBPACK_IMPORTED_MODULE_7__ = __webpack_require__(/*! @angular/material/icon */ "./node_modules/@angular/material/esm5/icon.es5.js");
/* harmony import */ var _angular_material_toolbar__WEBPACK_IMPORTED_MODULE_8__ = __webpack_require__(/*! @angular/material/toolbar */ "./node_modules/@angular/material/esm5/toolbar.es5.js");
/* harmony import */ var _angular_material_sidenav__WEBPACK_IMPORTED_MODULE_9__ = __webpack_require__(/*! @angular/material/sidenav */ "./node_modules/@angular/material/esm5/sidenav.es5.js");
/* harmony import */ var _angular_material_menu__WEBPACK_IMPORTED_MODULE_10__ = __webpack_require__(/*! @angular/material/menu */ "./node_modules/@angular/material/esm5/menu.es5.js");
/* harmony import */ var _angular_material_grid_list__WEBPACK_IMPORTED_MODULE_11__ = __webpack_require__(/*! @angular/material/grid-list */ "./node_modules/@angular/material/esm5/grid-list.es5.js");
/* harmony import */ var _angular_material_list__WEBPACK_IMPORTED_MODULE_12__ = __webpack_require__(/*! @angular/material/list */ "./node_modules/@angular/material/esm5/list.es5.js");
/* harmony import */ var _angular_material_tooltip__WEBPACK_IMPORTED_MODULE_13__ = __webpack_require__(/*! @angular/material/tooltip */ "./node_modules/@angular/material/esm5/tooltip.es5.js");
/* harmony import */ var _angular_material_divider__WEBPACK_IMPORTED_MODULE_14__ = __webpack_require__(/*! @angular/material/divider */ "./node_modules/@angular/material/esm5/divider.es5.js");















var list = [
    _angular_common__WEBPACK_IMPORTED_MODULE_2__["CommonModule"],
    _angular_material_button__WEBPACK_IMPORTED_MODULE_4__["MatButtonModule"],
    _angular_material_card__WEBPACK_IMPORTED_MODULE_3__["MatCardModule"],
    _angular_material_input__WEBPACK_IMPORTED_MODULE_5__["MatInputModule"],
    _angular_material_form_field__WEBPACK_IMPORTED_MODULE_6__["MatFormFieldModule"],
    _angular_material_icon__WEBPACK_IMPORTED_MODULE_7__["MatIconModule"],
    _angular_material_toolbar__WEBPACK_IMPORTED_MODULE_8__["MatToolbarModule"],
    _angular_material_sidenav__WEBPACK_IMPORTED_MODULE_9__["MatSidenavModule"],
    _angular_material_menu__WEBPACK_IMPORTED_MODULE_10__["MatMenuModule"],
    _angular_material_grid_list__WEBPACK_IMPORTED_MODULE_11__["MatGridListModule"],
    _angular_material_list__WEBPACK_IMPORTED_MODULE_12__["MatListModule"],
    _angular_material_tooltip__WEBPACK_IMPORTED_MODULE_13__["MatTooltipModule"],
    _angular_material_divider__WEBPACK_IMPORTED_MODULE_14__["MatDividerModule"]
];
var MaterialModule = /** @class */ (function () {
    function MaterialModule() {
    }
    MaterialModule = tslib__WEBPACK_IMPORTED_MODULE_0__["__decorate"]([
        Object(_angular_core__WEBPACK_IMPORTED_MODULE_1__["NgModule"])({
            declarations: [],
            imports: list,
            exports: list
        })
    ], MaterialModule);
    return MaterialModule;
}());



/***/ }),

/***/ "./src/app/core/services/account.service.ts":
/*!**************************************************!*\
  !*** ./src/app/core/services/account.service.ts ***!
  \**************************************************/
/*! exports provided: AccountService */
/***/ (function(module, __webpack_exports__, __webpack_require__) {

"use strict";
__webpack_require__.r(__webpack_exports__);
/* harmony export (binding) */ __webpack_require__.d(__webpack_exports__, "AccountService", function() { return AccountService; });
/* harmony import */ var tslib__WEBPACK_IMPORTED_MODULE_0__ = __webpack_require__(/*! tslib */ "./node_modules/tslib/tslib.es6.js");
/* harmony import */ var _angular_core__WEBPACK_IMPORTED_MODULE_1__ = __webpack_require__(/*! @angular/core */ "./node_modules/@angular/core/fesm5/core.js");
/* harmony import */ var _angular_fire_firestore__WEBPACK_IMPORTED_MODULE_2__ = __webpack_require__(/*! @angular/fire/firestore */ "./node_modules/@angular/fire/firestore/index.js");



var AccountService = /** @class */ (function () {
    /**
     * Used to get public info about users
     * @param afs angularfirestore (injected)
     */
    function AccountService(afs) {
        this.afs = afs;
        /**
         * the map for caching users
         */
        this.users = new Map();
    }
    /**
     * Used to get an user knowing its id
     * @param uid The id of the user requested
     * @returns the user observable
     * @example getUser("438239472433")
     */
    AccountService.prototype.getUser = function (uid) {
        //check to see if we already have the observable for the user
        if (this.users.get(uid))
            return this.users.get(uid);
        //if we dont have it than get it from the db
        var user = this.afs.doc("users/" + uid).valueChanges();
        //cache the observable in the map
        this.users.set(uid, user);
        //return the user
        return user;
    };
    AccountService = tslib__WEBPACK_IMPORTED_MODULE_0__["__decorate"]([
        Object(_angular_core__WEBPACK_IMPORTED_MODULE_1__["Injectable"])({
            providedIn: 'root'
        }),
        tslib__WEBPACK_IMPORTED_MODULE_0__["__metadata"]("design:paramtypes", [_angular_fire_firestore__WEBPACK_IMPORTED_MODULE_2__["AngularFirestore"]])
    ], AccountService);
    return AccountService;
}());



/***/ }),

/***/ "./src/app/core/services/auth.service.ts":
/*!***********************************************!*\
  !*** ./src/app/core/services/auth.service.ts ***!
  \***********************************************/
/*! exports provided: AuthService */
/***/ (function(module, __webpack_exports__, __webpack_require__) {

"use strict";
__webpack_require__.r(__webpack_exports__);
/* harmony export (binding) */ __webpack_require__.d(__webpack_exports__, "AuthService", function() { return AuthService; });
/* harmony import */ var tslib__WEBPACK_IMPORTED_MODULE_0__ = __webpack_require__(/*! tslib */ "./node_modules/tslib/tslib.es6.js");
/* harmony import */ var _angular_core__WEBPACK_IMPORTED_MODULE_1__ = __webpack_require__(/*! @angular/core */ "./node_modules/@angular/core/fesm5/core.js");
/* harmony import */ var rxjs__WEBPACK_IMPORTED_MODULE_2__ = __webpack_require__(/*! rxjs */ "./node_modules/rxjs/_esm5/index.js");
/* harmony import */ var rxjs_operators__WEBPACK_IMPORTED_MODULE_3__ = __webpack_require__(/*! rxjs/operators */ "./node_modules/rxjs/_esm5/operators/index.js");
/* harmony import */ var _angular_fire_auth__WEBPACK_IMPORTED_MODULE_4__ = __webpack_require__(/*! @angular/fire/auth */ "./node_modules/@angular/fire/auth/index.js");
/* harmony import */ var _angular_fire_firestore__WEBPACK_IMPORTED_MODULE_5__ = __webpack_require__(/*! @angular/fire/firestore */ "./node_modules/@angular/fire/firestore/index.js");
/* harmony import */ var _angular_router__WEBPACK_IMPORTED_MODULE_6__ = __webpack_require__(/*! @angular/router */ "./node_modules/@angular/router/fesm5/router.js");
/* harmony import */ var firebase__WEBPACK_IMPORTED_MODULE_7__ = __webpack_require__(/*! firebase */ "./node_modules/firebase/dist/index.cjs.js");
/* harmony import */ var firebase__WEBPACK_IMPORTED_MODULE_7___default = /*#__PURE__*/__webpack_require__.n(firebase__WEBPACK_IMPORTED_MODULE_7__);








var AuthService = /** @class */ (function () {
    function AuthService(auth, afs, router) {
        var _this = this;
        this.auth = auth;
        this.afs = afs;
        this.router = router;
        this.persistence = firebase__WEBPACK_IMPORTED_MODULE_7__["auth"].Auth.Persistence.LOCAL;
        this.user = this.auth.authState.pipe(Object(rxjs_operators__WEBPACK_IMPORTED_MODULE_3__["switchMap"])(function (user) {
            // console.log(`User:${user}`)
            if (user) {
                // console.log(user)
                return _this.afs.doc("users/" + user.uid).valueChanges();
            }
            return Object(rxjs__WEBPACK_IMPORTED_MODULE_2__["of"])(null);
        }));
        this.providers = {
            "google": function () { return new firebase__WEBPACK_IMPORTED_MODULE_7__["auth"].GoogleAuthProvider(); },
            "github": function () { return new firebase__WEBPACK_IMPORTED_MODULE_7__["auth"].GithubAuthProvider(); },
            "facebook": function () { return new firebase__WEBPACK_IMPORTED_MODULE_7__["auth"].FacebookAuthProvider(); }
        };
    }
    AuthService.prototype.rawLogin = function (email, password) {
        return tslib__WEBPACK_IMPORTED_MODULE_0__["__awaiter"](this, void 0, void 0, function () {
            var credential;
            return tslib__WEBPACK_IMPORTED_MODULE_0__["__generator"](this, function (_a) {
                switch (_a.label) {
                    case 0: return [4 /*yield*/, this.auth.auth.signInWithEmailAndPassword(email, password)];
                    case 1:
                        credential = _a.sent();
                        return [2 /*return*/, this.updateUserData(credential.user)];
                }
            });
        });
    };
    AuthService.prototype.login = function (provider) {
        return tslib__WEBPACK_IMPORTED_MODULE_0__["__awaiter"](this, void 0, void 0, function () {
            var credential, err_1;
            return tslib__WEBPACK_IMPORTED_MODULE_0__["__generator"](this, function (_a) {
                switch (_a.label) {
                    case 0:
                        _a.trys.push([0, 3, , 4]);
                        if (!this.providers[provider])
                            throw new Error("unknown provider");
                        return [4 /*yield*/, this.auth.auth.setPersistence(this.persistence)];
                    case 1:
                        _a.sent();
                        return [4 /*yield*/, this.auth.auth.signInWithPopup(this.providers[provider]())];
                    case 2:
                        credential = _a.sent();
                        this.updateUserData(credential.user);
                        return [3 /*break*/, 4];
                    case 3:
                        err_1 = _a.sent();
                        console.error(err_1);
                        return [3 /*break*/, 4];
                    case 4: return [2 /*return*/];
                }
            });
        });
    };
    AuthService.prototype.updateUserData = function (user) {
        var userRef = this.afs.doc("users/" + user.uid);
        var data = {
            uid: user.uid,
            email: user.email,
            photoUrl: user.photoURL,
            displayName: user.displayName
        };
        return userRef.set(data, { merge: true });
    };
    /**
     * used to update the user
     * @param uid the id of the user to update
     * @param data the data to update nside the user
     */
    AuthService.prototype.updateUser = function (uid, data) {
        var userRef = this.afs.doc("users/" + uid);
        return userRef.update(data);
    };
    AuthService.prototype.logout = function () {
        this.auth.auth.signOut();
    };
    AuthService = tslib__WEBPACK_IMPORTED_MODULE_0__["__decorate"]([
        Object(_angular_core__WEBPACK_IMPORTED_MODULE_1__["Injectable"])({
            providedIn: 'root'
        }),
        tslib__WEBPACK_IMPORTED_MODULE_0__["__metadata"]("design:paramtypes", [_angular_fire_auth__WEBPACK_IMPORTED_MODULE_4__["AngularFireAuth"],
            _angular_fire_firestore__WEBPACK_IMPORTED_MODULE_5__["AngularFirestore"],
            _angular_router__WEBPACK_IMPORTED_MODULE_6__["Router"]])
    ], AuthService);
    return AuthService;
}());



/***/ }),

/***/ "./src/app/core/services/friend-system.service.ts":
/*!********************************************************!*\
  !*** ./src/app/core/services/friend-system.service.ts ***!
  \********************************************************/
/*! exports provided: FriendSystemService */
/***/ (function(module, __webpack_exports__, __webpack_require__) {

"use strict";
__webpack_require__.r(__webpack_exports__);
/* harmony export (binding) */ __webpack_require__.d(__webpack_exports__, "FriendSystemService", function() { return FriendSystemService; });
/* harmony import */ var tslib__WEBPACK_IMPORTED_MODULE_0__ = __webpack_require__(/*! tslib */ "./node_modules/tslib/tslib.es6.js");
/* harmony import */ var _angular_core__WEBPACK_IMPORTED_MODULE_1__ = __webpack_require__(/*! @angular/core */ "./node_modules/@angular/core/fesm5/core.js");
/* harmony import */ var _angular_fire_firestore__WEBPACK_IMPORTED_MODULE_2__ = __webpack_require__(/*! @angular/fire/firestore */ "./node_modules/@angular/fire/firestore/index.js");
/* harmony import */ var _auth_service__WEBPACK_IMPORTED_MODULE_3__ = __webpack_require__(/*! ./auth.service */ "./src/app/core/services/auth.service.ts");
/* harmony import */ var _account_service__WEBPACK_IMPORTED_MODULE_4__ = __webpack_require__(/*! ./account.service */ "./src/app/core/services/account.service.ts");
/* harmony import */ var rxjs_operators__WEBPACK_IMPORTED_MODULE_5__ = __webpack_require__(/*! rxjs/operators */ "./node_modules/rxjs/_esm5/operators/index.js");






var FriendSystemService = /** @class */ (function () {
    /**
     * the system used to querry friends
     * @param afs the angular firestore db (injected)
     */
    function FriendSystemService(auth, afs, pas) {
        var _this = this;
        this.auth = auth;
        this.afs = afs;
        this.pas = pas;
        //subscribe to the user observable
        this.auth.user.subscribe(function (user) {
            if (!user) {
                _this.friends = [];
                _this.uids = [];
            }
            //in case it doesnt have any friends make it an epty array
            if (!user.friends)
                return _this.auth.updateUser(user.uid, {
                    friends: []
                });
            //update the user list
            _this.uids = user.friends;
            _this.friends = _this.uids.map(function (uid) { return _this.pas.getUser(uid); });
        });
    }
    /**
     * used to remove friends
     * @param uid the uid of the friend to remove
     * @throws UndefinedFriendError when unable to remove friend, because it isnt in the friend list
     * @returns a promise resolving after the data is succesfully saved to the database
     */
    FriendSystemService.prototype.removeFriend = function (uid) {
        var _this = this;
        return new Promise(function (resolve, reject) {
            //check if uid is inside the friend list
            if (_this.uids.indexOf(uid) == -1)
                reject("UndefinedFriendError: unable to remove friend " + uid + ", because it isnt in the friend list");
            //subscirbe to the user (and only take one)
            _this.auth.user.pipe(Object(rxjs_operators__WEBPACK_IMPORTED_MODULE_5__["take"])(1)).subscribe(function (user) { return tslib__WEBPACK_IMPORTED_MODULE_0__["__awaiter"](_this, void 0, void 0, function () {
                return tslib__WEBPACK_IMPORTED_MODULE_0__["__generator"](this, function (_a) {
                    switch (_a.label) {
                        case 0: 
                        //remove the friend from the db
                        return [4 /*yield*/, this.auth.updateUser(user.uid, {
                                friends: this.uids.filter(function (value) { return value != uid; })
                            })];
                        case 1:
                            //remove the friend from the db
                            _a.sent();
                            resolve(this.friends);
                            return [2 /*return*/];
                    }
                });
            }); });
        });
    };
    /**
     * used to add a friend
     * @param uid the uid of the friend to add
     */
    FriendSystemService.prototype.addFriend = function (uid) {
        var _this = this;
        //return a promise
        return new Promise(function (resolve, reject) {
            //just some client side security 
            //(i have to also add back end security rules)
            if (_this.uids.indexOf(uid) != -1)
                reject("InvalidNewFriendError: cannot add friend with uid " + uid + ". A friend with the same uid already exists");
            //subscribe to the user observable to get the uid (and only take 1)
            _this.auth.user.pipe(Object(rxjs_operators__WEBPACK_IMPORTED_MODULE_5__["take"])(1)).subscribe(function (user) { return tslib__WEBPACK_IMPORTED_MODULE_0__["__awaiter"](_this, void 0, void 0, function () {
                return tslib__WEBPACK_IMPORTED_MODULE_0__["__generator"](this, function (_a) {
                    switch (_a.label) {
                        case 0: 
                        //save it into the db
                        return [4 /*yield*/, this.auth.updateUser(user.uid, {
                                friends: this.uids.concat([uid])
                            })];
                        case 1:
                            //save it into the db
                            _a.sent();
                            resolve(this.friends);
                            return [2 /*return*/];
                    }
                });
            }); });
        });
    };
    FriendSystemService = tslib__WEBPACK_IMPORTED_MODULE_0__["__decorate"]([
        Object(_angular_core__WEBPACK_IMPORTED_MODULE_1__["Injectable"])({
            providedIn: 'root'
        }),
        tslib__WEBPACK_IMPORTED_MODULE_0__["__metadata"]("design:paramtypes", [_auth_service__WEBPACK_IMPORTED_MODULE_3__["AuthService"],
            _angular_fire_firestore__WEBPACK_IMPORTED_MODULE_2__["AngularFirestore"],
            _account_service__WEBPACK_IMPORTED_MODULE_4__["AccountService"]])
    ], FriendSystemService);
    return FriendSystemService;
}());



/***/ }),

/***/ "./src/environments/environment.ts":
/*!*****************************************!*\
  !*** ./src/environments/environment.ts ***!
  \*****************************************/
/*! exports provided: environment */
/***/ (function(module, __webpack_exports__, __webpack_require__) {

"use strict";
__webpack_require__.r(__webpack_exports__);
/* harmony export (binding) */ __webpack_require__.d(__webpack_exports__, "environment", function() { return environment; });
// This file can be replaced during build by using the `fileReplacements` array.
// `ng build --prod` replaces `environment.ts` with `environment.prod.ts`.
// The list of file replacements can be found in `angular.json`.
var environment = {
    production: false,
    firebase: {
        apiKey: "AIzaSyBMhf35HijKKSRZdRYt_eBNLGadhQKdZVY",
        authDomain: "planets-io.firebaseapp.com",
        databaseURL: "https://planets-io.firebaseio.com",
        projectId: "planets-io",
        storageBucket: "planets-io.appspot.com",
        messagingSenderId: "209812924936"
    }
};
/*
 * For easier debugging in development mode, you can import the following file
 * to ignore zone related error stack frames such as `zone.run`, `zoneDelegate.invokeTask`.
 *
 * This import should be commented out in production mode because it will have a negative impact
 * on performance if an error is thrown.
 */
// import 'zone.js/dist/zone-error';  // Included with Angular CLI.


/***/ }),

/***/ "./src/main.ts":
/*!*********************!*\
  !*** ./src/main.ts ***!
  \*********************/
/*! no exports provided */
/***/ (function(module, __webpack_exports__, __webpack_require__) {

"use strict";
__webpack_require__.r(__webpack_exports__);
/* harmony import */ var hammerjs__WEBPACK_IMPORTED_MODULE_0__ = __webpack_require__(/*! hammerjs */ "./node_modules/hammerjs/hammer.js");
/* harmony import */ var hammerjs__WEBPACK_IMPORTED_MODULE_0___default = /*#__PURE__*/__webpack_require__.n(hammerjs__WEBPACK_IMPORTED_MODULE_0__);
/* harmony import */ var _angular_core__WEBPACK_IMPORTED_MODULE_1__ = __webpack_require__(/*! @angular/core */ "./node_modules/@angular/core/fesm5/core.js");
/* harmony import */ var _angular_platform_browser_dynamic__WEBPACK_IMPORTED_MODULE_2__ = __webpack_require__(/*! @angular/platform-browser-dynamic */ "./node_modules/@angular/platform-browser-dynamic/fesm5/platform-browser-dynamic.js");
/* harmony import */ var _app_app_module__WEBPACK_IMPORTED_MODULE_3__ = __webpack_require__(/*! ./app/app.module */ "./src/app/app.module.ts");
/* harmony import */ var _environments_environment__WEBPACK_IMPORTED_MODULE_4__ = __webpack_require__(/*! ./environments/environment */ "./src/environments/environment.ts");





if (_environments_environment__WEBPACK_IMPORTED_MODULE_4__["environment"].production) {
    Object(_angular_core__WEBPACK_IMPORTED_MODULE_1__["enableProdMode"])();
}
Object(_angular_platform_browser_dynamic__WEBPACK_IMPORTED_MODULE_2__["platformBrowserDynamic"])().bootstrapModule(_app_app_module__WEBPACK_IMPORTED_MODULE_3__["AppModule"])
    .catch(function (err) { return console.error(err); });


/***/ }),

/***/ 0:
/*!***************************!*\
  !*** multi ./src/main.ts ***!
  \***************************/
/*! no static exports found */
/***/ (function(module, exports, __webpack_require__) {

module.exports = __webpack_require__(/*! c:\Users\larym\repos\angular-client\src\main.ts */"./src/main.ts");


/***/ })

},[[0,"runtime","vendor"]]]);
//# sourceMappingURL=main.js.map