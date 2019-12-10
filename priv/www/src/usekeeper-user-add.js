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
import '@polymer/paper-dialog/paper-dialog.js';
import '@polymer/paper-toolbar/paper-toolbar.js';
import '@polymer/paper-progress/paper-progress.js';
import '@polymer/paper-input/paper-input.js';
import '@polymer/paper-button/paper-button.js';
import './style-element.js';

class userAdd extends PolymerElement {
	static get template() {
		return html`
		<style include="style-element"></style>
		<paper-dialog class="dialog" id="userAddModal" modal>
			<paper-toolbar>
				<div slot="top"><h2>Add User</h2></div>
			</paper-toolbar>
			<paper-progress
					indeterminate
					class="slow red"
					disabled="{{!loading}}">
			</paper-progress>
				<paper-input
					label="Username"
					value="{{userUsername}}">
				</paper-input>
				<paper-input
					label="Password"
					value="{{userPassword}}">
				</paper-input>
				<paper-input
					label="First Name"
					value="{{userFirstName}}">
				</paper-input>
				<paper-input
					label="Last Name"
					value="{{userLastName}}">
				</paper-input>
				<div class="buttons">
					<paper-button
							raised
							class="submit-button"
							on-tap="_add">
						Add
					</paper-button>
					<paper-button
							class="cancel-button"
							on-tap="_cancel">
						Cancel
					</paper-button>
				</div>
		</paper-dialog>
		<iron-ajax
				id="userAddAjax"
				content-type="application/json"
				loading="{{loading}}"
				on-response="_response"
				on-error="_error">
		</iron-ajax>
		`;
	}

	static get properties() {
		return {
			loading: {
				type: Boolean,
				value: false
			},
			userUsername: {
				type: String
			},
			userPassword: {
				type: String
			},
			userFirsName: {
				type: String
			},
			userLastName: {
				type: String
			}
		}
	}

	ready() {
		super.ready()
	}

	_cancel() {
		this.userUsername = null;
		this.userPassword = null;
		this.userFirstName = null;
		this.userLastName = null;
	}

	_add() {
		var ajax = this.$.userAddAjax;
		ajax.method = "POST";
		ajax.url = "party/v4/individual";
		var user = new Object();
		user.characteristic = [];
		if(this.userUsername) {
			user.id = this.userUsername;
			var char1 = new Object();
			char1.name = "username";
			char1.value = this.userUsername;
			user.characteristic.push(char1);
		}
		if(this.userPassword) {
			var char2 = new Object();
			char2.name = "password";
			char2.value = this.userPassword;
			user.characteristic.push(char2);
		}
		if(this.userFirstName) {
			var char3 = new Object();
			char3.name = "givenName";
			char3.value = this.userFirstName;
			user.characteristic.push(char3);
		}
		if(this.userLastName) {
			var char4 = new Object();
			char4.name = "lastName";
			char4.value = this.userLastName;
			user.characteristic.push(char4);
		}
		ajax.body = JSON.stringify(user);
		ajax.generateRequest();
	}

	_response() {
		this.$.userAddModal.close();
		this.userUsername = null;
		this.userPassword = null;
		this.userFirstName = null;
		this.userLastName = null;
		document.body.querySelector('usekeeper-shell').shadowRoot.getElementById('userList').shadowRoot.getElementById('userGrid').clearCache();
	}

	_error(event) {
		var toast = document.body.querySelector('usekeeper-shell').shadowRoot.getElementById('restError');
		toast.text = event.detail.request.xhr.statusText;
		toast.open();
	}
}

window.customElements.define('usekeeper-user-add', userAdd);
