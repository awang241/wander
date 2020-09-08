<template>
  <div class="container containerColor">
    <div id="results" class="column" v-if="notifications.length">
      <div
          v-for="notification in notifications"
          :key="notification.time">
        <Notification :notification="notification">
        </Notification>
        <br>
      </div>
    </div>
    <div v-else id="noMatches">
      <h1>No Notifications found!</h1>
    </div>
    <observer v-on:intersect="$emit('loadMoreNotifications')"></observer>
  </div>
</template>

<script>
import store from "../store";
import toastMixin from "../mixins/toastMixin";
import Observer from "./Observer";
import api from "../Api";
import Notification from "../components/Notification";

export default {
  name: "HomeFeed",
  components: {
    Observer,
    Notification
  },
  mixins: [toastMixin],
  data() {
    return {
      store: store,
      observer: null,
      notifications: [],
    }
  },
  methods: {
      loadMoreNotifications(){
        const searchParameters = {count: 50, startIndex: 0}
        api.getNotifications(Number(this.store.getters.getUserId), localStorage.getItem("authToken"), searchParameters).then(response => {
            if(response.data.notifications.length > 0){
              this.notifications = response.data.notifications;
            }
        })
      }
  },
  mounted() {
    this.loadMoreNotifications()
      // Old Testing Data:
      // console.log(store.getters.getUserId);
      // for (let i = 1; i < 5; i++) {
      //     let notification = {
      //         message: `This is my own dummy notification #${i}`,
      //         notificationType: i,
      //         dateTime: "2020-09-05T08:00:00+1300",
      //         activityId: 45011,
      //         profileId: null
      //     };
      //     this.notifications.push(notification);
      // }
      // for (let i = 0; i < 11; i++) {
      //     let notification = {
      //         message: `This is dummy notification #${i}`,
      //         notificationType: i,
      //         dateTime: "2020-09-05T08:00:00+1300",
      //         activityId: 45011,
      //         profileId: 0
      //     };
      //     this.notifications.push(notification);
      // }

    //End of test data
  }
}
</script>

<style scoped>

</style>