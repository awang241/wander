<template>
    <div class="container">
        <h1 class="title is-5">Activity Search</h1>

      <div>
          insert reusable component

      </div>
        <MapPane marker-label="Profile Location" :info-window-content="activitesData" :location-choice-coordinates="profileLocationLatLong" v-on:locationChoiceChanged="updateLocation"></MapPane>
        <br>

        <div class="row">
            <br>
            <b-field style="float:left">
                <b-button type="is-danger" @click="clearLocation()">Clear</b-button>
            </b-field>
            <b-field style="float:right">
                <b-button type="is-primary" @click="search()">Search</b-button>
            </b-field>
            <br>
        </div>
        <br/>
    </div>
</template>

<script>
    import googleMapsInit from '../../utils/googlemaps'
    import MapPane from "../MapPane";
    import api from "../../Api";
    import router from "../../router";

    export default {
        name: "ActivitySearch",
        components: {
            MapPane
        },
        data() {
            return {
            }
        },
        methods: {
            clearLocation() {
                console.log("needs to be implemented")
            },
            search() {
                console.log("needs to be implemented")
            },
            setDefaultProfileLocation() {
                api.getProfile(this.id, localStorage.getItem('authToken'))
                    .then((response) => {
                        let location = response.data.location;
                        this.profileLocationLatLong = {lat: location.lat(), lng: location.lng()};
                    })
                    .catch(() => {
                        this.warningToast("Error occurred while getting Profile Location details.");
                        router.go(-1)
                    })
            },

            /**
             * Method to format the details of an activity for the information pop up window
             * At the moment it has dummy data
             * Need to put in a variable (activityDetails) into this method
             */
            formatActivityDetails() {

                //This variable is dummy data
                let activityDetails = {
                    activityName : "Doing happy tings",
                    location: "a happy place",
                    lat: 68.174270,
                    lng: 16.329620,
                    activityTypes: ["happy stuff", "really happy stuff"]
                };
                const activityTypesString = this.formatActivityTypesString(activityDetails.activityTypes);

                //Had to use inline styling because of scope :(
                const informationWindowText =
                    `<div style="width: 100vh; height: 100vh;">` +
                    `<h1 style="font-size: 22px">${activityDetails.activityName}</h1>` +
                    `<br>` +
                    `<h1 class="infoWindowHeader">Location: <span>${activityDetails.location}</span></h1>` +
                    `<br>` +
                    `<h1 class="infoWindowHeader">Latitude: <span>${activityDetails.lat}</span></h1>` +
                    `<br>` +
                    `<h1 class="infoWindowHeader">Longitude: <span>${activityDetails.lng}</span></h1>` +
                    `<br>` +
                    `${activityTypesString}` +
                    `</div>`
                return informationWindowText
            },

            formatActivityTypesString(activityTypes) {
                let formattedActivityTypes =
                    `<h1 style="font-size: 16px"> Activity Types:</h1>` +
                    `<br>`
                let typesString = "";
                for (let i = 0; i < activityTypes.length; i++) {
                    typesString =  typesString + `*<span>${activityTypes[i]}</span>` + `<br>`
                }
                return formattedActivityTypes + typesString
            }
        },
        async mounted() {
            this.google = await googleMapsInit();
            this.setDefaultProfileLocation();
        }
    }

</script>

<style scoped>

</style>