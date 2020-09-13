<template>
    <div class="container containerColor">
        <h1 class="title">Home</h1>
        <div id="results" class="column" v-if="notifications.length">
            <div v-for="notification in notifications"
                :key="notification.timeStamp">
            <Notification :notification="notification"/>
            <br>
            </div>
        </div>
        <div v-else id="noMatches">
            <h1>You have no notifications.</h1>
        </div>
        <div>
            <observer v-on:intersect="loadMoreNotifications()"/>
        </div>

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
      startIndex: 0,
      moreNotificationsExist: true
    }
  },
  methods: {
      loadMoreNotifications() {
        if (this.moreNotificationsExist) {
          const searchParameters = {count: 5, startIndex: this.startIndex};
          api.getNotifications(Number(this.store.getters.getUserId), localStorage.getItem("authToken"), searchParameters).then(response => {
            if (response.data.notifications.length > 0) {
              this.notifications = [...this.notifications, ...response.data.notifications];
              this.startIndex += response.data.notifications.length;
              if (response.data.notifications.length < 5) {
                  this.moreNotificationsExist = false;
              }
            } else {
              this.moreNotificationsExist = false;
            }
          })
        }

      }
  },
  mounted() {
    //this.loadMoreNotifications()
  }
}
</script>

<style scoped>

</style>