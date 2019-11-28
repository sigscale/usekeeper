/**
 * @license
 * Copyright (c) 2019 The Polymer Project Authors. All rights reserved.
 * This code may only be used under the BSD style license found at http://polymer.github.io/LICENSE.txt
 * The complete set of authors may be found at http://polymer.github.io/AUTHORS.txt
 * The complete set of contributors may be found at http://polymer.github.io/CONTRIBUTORS.txt
 * Code distributed by Google as part of the polymer project is also
 * subject to an additional IP rights grant found at http://polymer.github.io/PATENTS.txt
 */

import { PolymerElement, html } from '@polymer/polymer/polymer-element.js';
import '@polymer/iron-ajax/iron-ajax.js';
import '@polymer/paper-fab/paper-fab.js';
import '@polymer/iron-icons/iron-icons.js';
import '@polymer/paper-dialog/paper-dialog.js';
import '@polymer/paper-toolbar/paper-toolbar.js';
import '@polymer/paper-input/paper-input.js';
import '@polymer/paper-button/paper-button.js';
import '@polymer/paper-dropdown-menu/paper-dropdown-menu.js';
import '@polymer/paper-listbox/paper-listbox.js';
import '@polymer/paper-item/paper-item.js'
import '@polymer/iron-collapse/iron-collapse.js';
import './style-element.js';

class userAdd extends PolymerElement {
	static get template() {
		return html`
			<style include="style-element">
			</style>
		<paper-dialog class="dialog" id="addUserModal" modal>
			<paper-toolbar>
				<div slot="top"><h2>Add User</h2></div>
			</paper-toolbar>
				<paper-input
					id="username"
					label="User Name"
					value="{{user.username}}">
				</paper-input>
				<paper-input
					id="password"
					label="Password"
					value="{{user.password}}">
				</paper-input>
				<paper-input
					id="firstname"
					label="First Name"
					value="{{user.firstname}}">
				</paper-input>
				<paper-input
					id="lastname"
					label="Last Name"
					value="{{user.lastname}}">
				</paper-input>
				<div class="buttons">
					<paper-button
						raised
						class="submit-button"
						on-tap="_addUser">
							Add
					</paper-button>
					<paper-button
						class="cancel-button"
						dialog-dismiss
						on-tap="cancelSpec">
							Cancel
					</paper-button>
					<paper-button
						toggles
						raised
						class="delete-button"
						on-tap="_deleteSpec">
							Delete
					</paper-button>
				</div>
		</paper-dialog>
		<iron-ajax
			id="userAddAjax"
			content-type="application/json"
			on-loading-changed="_onLoadingChanged"
			on-response="_userAddResponse"
			on-error="_userAddError">
		</iron-ajax>
		`;
	}

	static get properties() {
		return {
			user: {
				type: Object,
			},
			charArray: {
				type: Array,
				value: function() {
					return [];
				}
			},
		}
	}

	ready() {
		super.ready()
	}

	_addUser() {
		var ajax = this.$.userAddAjax;
		ajax.method = "POST";
		ajax.url = "party/v4/individual";
		var use = new Object();
		if(this.$.username.value) {
			use.id = this.$.username.value;
		}
		if(this.$.username.value) {
			var charObj = new Object();
			charObj.name = "username";
			charObj.value = this.$.username.value;
		}
		if(this.$.password.value) {
			var charObj1 = new Object();
			charObj1.name = "password";
			charObj1.value = this.$.password.value;
		}
		if(this.$.firstname.value) {
			var charObj2 = new Object();
			charObj2.name = "givenName";
			charObj2.value = this.$.firstname.value;
		}
		if(this.$.lastname.value) {
			var charObj3 = new Object();
			charObj3.name = "lastName";
			charObj3.value = this.$.lastname.value;
		}
		use.characteristic = [charObj, charObj1, charObj2, charObj3];
		ajax.body = JSON.stringify(use);
		ajax.generateRequest();
	}

	_userAddResponse() {
		document.body.querySelector('usekeeper-shell').shadowRoot.querySelector('usekeeper-user-add').shadowRoot.getElementById('addUserModal').close();
	}
}

window.customElements.define('usekeeper-user-add', userAdd);
